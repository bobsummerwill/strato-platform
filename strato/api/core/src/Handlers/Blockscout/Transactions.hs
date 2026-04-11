{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Transactions
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (BlockHeader(..))
import Blockchain.Data.DataDefs (RawTransaction(..), TransactionResult(..))
import Blockchain.Data.Transaction (transactionHash)
import qualified Blockchain.Data.TransactionResultStatus as TRS
import Blockchain.Model.JsonBlock (bPrimeToB, rtPrimeToRt)
import Blockchain.Strato.Model.Address (Address)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.Keccak256 (Keccak256, keccak256ToHex)
import Control.Monad.Change.Alter (Selectable, selectMany)
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.List (findIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Handlers.Block (BlocksFilterParams(..), blocksFilterParams, getBlockInfo')
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Handlers.Transaction (TxsFilterParams(..), getTransaction', txsFilterParams)
import Servant
import Text.Format (format)

type API =
  (    "transactions" :> "by-hash" :> ReqBody '[JSON] TransactionHashesRequest :> Post '[JSON] Value
  :<|> "transactions" :> "count" :> "by-block-number" :> ReqBody '[JSON] BlockNumbersRequest :> Post '[JSON] Value
  :<|> "transactions" :> "first-trace" :> ReqBody '[JSON] [FirstTraceLookup] :> Post '[JSON] Value
  :<|> "transactions" :> "raw-traces" :> ReqBody '[JSON] TransactionHashRequest :> Post '[JSON] Value
  )

server ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable TxsFilterParams [RawTransaction] m
  , Selectable Keccak256 [TransactionResult] m
  ) =>
  ServerT API m
server = byHash :<|> countByBlockNumber :<|> firstTrace :<|> rawTraces

byHash :: (Selectable BlocksFilterParams [Block] m, Selectable TxsFilterParams [RawTransaction] m) => TransactionHashesRequest -> m Value
byHash TransactionHashesRequest{requestedTransactionHashes} = do
  transactions <- mapM fetchTransaction requestedTransactionHashes
  pure $ object
    [ "transactions" .= transactions
    , "errors" .= ([] :: [Value])
    ]

countByBlockNumber :: (Selectable BlocksFilterParams [Block] m) => BlockNumbersRequest -> m Value
countByBlockNumber BlockNumbersRequest{requestedBlockNumbers} = do
  counts <- mapM blockCount requestedBlockNumbers
  pure $ Mapper.transactionCountValue counts

firstTrace ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable TxsFilterParams [RawTransaction] m
  , Selectable Keccak256 [TransactionResult] m
  ) =>
  [FirstTraceLookup] ->
  m Value
firstTrace lookups = do
  txResults <- fetchTransactionResultMap (map lookupHashData lookups)
  resolved <- mapM (buildFirstTraceItem txResults) lookups
  let items = [item | Right item <- resolved]
      errors = [err | Left err <- resolved]
  pure $ object
    [ "items" .= items
    , "errors" .= errors
    ]

rawTraces ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable TxsFilterParams [RawTransaction] m
  , Selectable Keccak256 [TransactionResult] m
  ) =>
  TransactionHashRequest ->
  m Value
rawTraces TransactionHashRequest{requestedTransactionHash} = do
  maybeRawTx <- fetchRawTransaction requestedTransactionHash
  txResults <- fetchTransactionResultMap [requestedTransactionHash]
  let maybeTxResult = listToMaybe =<< M.lookup requestedTransactionHash txResults
  case (maybeRawTx, maybeTxResult) of
    (Just rawTx, Just txResult) -> do
      context <- lookupContext rawTx
      pure $ object
        [ "traces" .= [rawTraceValue context rawTx txResult]
        , "errors" .= ([] :: [Value])
        ]
    (Nothing, _) ->
      pure $ object
        [ "traces" .= ([] :: [Value])
        , "errors" .= [Mapper.errorItem $ "missing transaction for 0x" <> textHex requestedTransactionHash]
        ]
    (_, Nothing) ->
      pure $ object
        [ "traces" .= ([] :: [Value])
        , "errors" .= [Mapper.errorItem $ "missing transaction result for 0x" <> textHex requestedTransactionHash]
        ]

fetchTransaction :: (Selectable BlocksFilterParams [Block] m, Selectable TxsFilterParams [RawTransaction] m) => Keccak256 -> m (Maybe Value)
fetchTransaction txHash = do
  txs <- map rtPrimeToRt <$> getTransaction' txsFilterParams{qtHash = Just txHash}
  case txs of
    [] -> pure Nothing
    rawTx : _ -> do
      context <- lookupContext rawTx
      pure $ Just $ Mapper.transactionValueFromRaw context rawTx

fetchRawTransaction :: Selectable TxsFilterParams [RawTransaction] m => Keccak256 -> m (Maybe RawTransaction)
fetchRawTransaction txHash = listToMaybe . map rtPrimeToRt <$> getTransaction' txsFilterParams{qtHash = Just txHash}

fetchTransactionResultMap :: Selectable Keccak256 [TransactionResult] m => [Keccak256] -> m (M.Map Keccak256 [TransactionResult])
fetchTransactionResultMap [] = pure M.empty
fetchTransactionResultMap hashes = selectMany (Proxy @[TransactionResult]) hashes

lookupContext :: (Selectable BlocksFilterParams [Block] m) => RawTransaction -> m (Maybe Mapper.TxContext)
lookupContext RawTransaction{rawTransactionBlockNumber, rawTransactionTxHash}
  | rawTransactionBlockNumber < 0 = pure Nothing
  | otherwise = do
      blocks <- map bPrimeToB <$> getBlockInfo' blocksFilterParams{qbNumber = Just (fromIntegral rawTransactionBlockNumber)}
      pure $ do
        block@Block{blockBlockData, blockReceiptTransactions} <- listToMaybe blocks
        index <- findIndex ((== rawTransactionTxHash) . transactionHash) blockReceiptTransactions
        pure $ Mapper.TxContext
          { Mapper.txContextBlockHash = blockHash block
          , Mapper.txContextBlockNumber = number blockBlockData
          , Mapper.txContextIndex = index
          }

blockCount :: (Selectable BlocksFilterParams [Block] m) => Integer -> m (Integer, Int)
blockCount blockNumber = do
  blocks <- map bPrimeToB <$> getBlockInfo' blocksFilterParams{qbNumber = Just (fromIntegral blockNumber)}
  let count = maybe 0 (length . blockReceiptTransactions) (listToMaybe blocks)
  pure (blockNumber, count)

buildFirstTraceItem ::
  (Selectable BlocksFilterParams [Block] m, Selectable TxsFilterParams [RawTransaction] m) =>
  M.Map Keccak256 [TransactionResult] ->
  FirstTraceLookup ->
  m (Either Value Value)
buildFirstTraceItem txResults lookupParams@FirstTraceLookup{lookupHashData} = do
  maybeRawTx <- fetchRawTransaction lookupHashData
  case (maybeRawTx, listToMaybe =<< M.lookup lookupHashData txResults) of
    (Just rawTx, Just txResult) -> do
      context <- lookupContext rawTx
      pure . Right $ firstTraceEnvelope lookupParams context rawTx txResult
    (Nothing, _) -> pure . Left $ Mapper.errorItem $ "missing transaction for 0x" <> textHex lookupHashData
    (_, Nothing) -> pure . Left $ Mapper.errorItem $ "missing transaction result for 0x" <> textHex lookupHashData

firstTraceEnvelope :: FirstTraceLookup -> Maybe Mapper.TxContext -> RawTransaction -> TransactionResult -> Value
firstTraceEnvelope lookupParams mContext rawTx txResult =
  object
    [ "block_hash" .= hexKeccak (blockHashFor lookupParams mContext txResult)
    , "block_number" .= blockNumberFor lookupParams mContext rawTx
    , "first_trace" .= firstTraceValue lookupParams mContext rawTx txResult
    ]

firstTraceValue :: FirstTraceLookup -> Maybe Mapper.TxContext -> RawTransaction -> TransactionResult -> Value
firstTraceValue FirstTraceLookup{lookupTransactionIndex} mContext RawTransaction{..} txResult =
  object (baseFields ++ typeFields ++ resultFields)
  where
    isCreate = rawTransactionToAddress == Nothing
    traceType = if isCreate then ("create" :: T.Text) else "call"
    txIndex = maybe lookupTransactionIndex Mapper.txContextIndex mContext
    inputData = maybe (Just "0x") (Just . hexBytes) rawTransactionTxData
    success = transactionResultStatus txResult == Just TRS.Success

    baseFields =
      [ "transaction_hash" .= hexKeccak rawTransactionTxHash
      , "trace_address" .= ([] :: [Int])
      , "index" .= (0 :: Int)
      , "transaction_index" .= txIndex
      , "from_address_hash" .= hexAddress rawTransactionFromAddress
      , "gas" .= rawTransactionGasLimit
      , "value" .= fromMaybe 0 rawTransactionValue
      , "type" .= traceType
      ]

    typeFields =
      if isCreate
        then
          [ "call_type" .= (Nothing :: Maybe T.Text)
          , "to_address_hash" .= (Nothing :: Maybe T.Text)
          , "created_contract_address_hash" .= fmap hexAddress (listToMaybe $ transactionResultContractsCreated txResult)
          , "init" .= inputData
          , "input" .= (Nothing :: Maybe T.Text)
          , "created_contract_code" .= (Nothing :: Maybe T.Text)
          ]
        else
          [ "call_type" .= Just ("call" :: T.Text)
          , "to_address_hash" .= fmap hexAddress rawTransactionToAddress
          , "created_contract_address_hash" .= (Nothing :: Maybe T.Text)
          , "init" .= (Nothing :: Maybe T.Text)
          , "input" .= inputData
          , "created_contract_code" .= (Nothing :: Maybe T.Text)
          ]

    resultFields
      | success =
          [ "gas_used" .= toInteger (transactionResultGasUsed txResult)
          , "output" .= normalizeOptionalHex (T.pack $ transactionResultResponse txResult)
          ]
      | otherwise =
          [ "error" .= T.pack (transactionResultMessage txResult)
          , "output" .= (Nothing :: Maybe T.Text)
          ]

rawTraceValue :: Maybe Mapper.TxContext -> RawTransaction -> TransactionResult -> Value
rawTraceValue mContext RawTransaction{..} txResult =
  object
    [ "transaction_hash" .= hexKeccak rawTransactionTxHash
    , "block_hash" .= hexKeccak (maybe (transactionResultBlockHash txResult) Mapper.txContextBlockHash mContext)
    , "block_number" .= maybe (toInteger rawTransactionBlockNumber) Mapper.txContextBlockNumber mContext
    , "transaction_index" .= fmap Mapper.txContextIndex mContext
    , "status" .= if transactionResultStatus txResult == Just TRS.Success then ("success" :: T.Text) else "failure"
    , "message" .= T.pack (transactionResultMessage txResult)
    , "response" .= normalizeOptionalHex (T.pack $ transactionResultResponse txResult)
    , "trace" .= T.pack (transactionResultTrace txResult)
    ]

blockHashFor :: FirstTraceLookup -> Maybe Mapper.TxContext -> TransactionResult -> Keccak256
blockHashFor FirstTraceLookup{lookupBlockHash} mContext txResult =
  fromMaybe (maybe (transactionResultBlockHash txResult) Mapper.txContextBlockHash mContext) lookupBlockHash

blockNumberFor :: FirstTraceLookup -> Maybe Mapper.TxContext -> RawTransaction -> Integer
blockNumberFor FirstTraceLookup{lookupBlockNumber} mContext RawTransaction{rawTransactionBlockNumber} =
  case mContext of
    Just context -> Mapper.txContextBlockNumber context
    Nothing -> max lookupBlockNumber (toInteger rawTransactionBlockNumber)

normalizeOptionalHex :: T.Text -> Maybe T.Text
normalizeOptionalHex rawText
  | T.null trimmed = Nothing
  | T.isPrefixOf "0x" trimmed = Just trimmed
  | T.all isHexDigit trimmed = Just $ "0x" <> trimmed
  | otherwise = Nothing
  where
    trimmed = T.strip rawText

hexAddress :: Address -> T.Text
hexAddress address = T.pack $ "0x" ++ show address

hexBytes :: BS.ByteString -> T.Text
hexBytes bytes = T.pack $ "0x" ++ format bytes

hexKeccak :: Keccak256 -> T.Text
hexKeccak value = T.pack $ "0x" ++ keccak256ToHex value

textHex :: Keccak256 -> T.Text
textHex = T.pack . keccak256ToHex
