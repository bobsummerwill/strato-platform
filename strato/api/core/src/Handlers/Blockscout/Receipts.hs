{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Receipts
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (number)
import Blockchain.Data.DataDefs (LogDB, TransactionResult(..))
import Blockchain.Data.Transaction (Transaction(..), transactionHash)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.Keccak256 (Keccak256, keccak256ToHex)
import Control.Monad.Change.Alter (Selectable, selectMany)
import Data.Aeson (Value)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (catMaybes, listToMaybe)
import Handlers.Block (BlocksFilterParams)
import qualified Handlers.Blockscout.Blocks as Blocks
import qualified Handlers.Blockscout.Logs as Logs
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Servant

type API =
  (    "receipts" :> "by-block-number" :> ReqBody '[JSON] BlockNumbersRequest :> Post '[JSON] Value
  :<|> "receipts" :> "by-transaction-hash" :> ReqBody '[JSON] TransactionHashesRequest :> Post '[JSON] Value
  )

server ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  ServerT API m
server = byBlockNumber :<|> byTransactionHash

byBlockNumber ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  BlockNumbersRequest ->
  m Value
byBlockNumber BlockNumbersRequest{requestedBlockNumbers} = do
  blocks <- catMaybes <$> mapM Blocks.fetchBlockByNumber requestedBlockNumbers
  buildReceiptResponse blocks (allTransactionHashes blocks)

byTransactionHash ::
  ( Selectable BlocksFilterParams [Block] m
  , Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  TransactionHashesRequest ->
  m Value
byTransactionHash TransactionHashesRequest{requestedTransactionHashes} = do
  requestedTxResults <- selectMany (Proxy @[TransactionResult]) requestedTransactionHashes
  let requestedResults = [ (txHash, listToMaybe =<< M.lookup txHash requestedTxResults) | txHash <- requestedTransactionHashes ]
      blockHashes = M.keys . M.fromList $ do
        (_, maybeResult) <- requestedResults
        txResult <- maybeToList maybeResult
        pure (transactionResultBlockHash txResult, ())
  blocks <- catMaybes <$> mapM Blocks.fetchBlockByHash blockHashes
  let missingErrors =
        [ Mapper.errorItem $ "missing transaction result for 0x" <> textHex txHash
        | (txHash, Nothing) <- requestedResults
        ]
  buildReceiptResponseWithErrors blocks requestedTransactionHashes missingErrors

buildReceiptResponse ::
  ( Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  [Block] ->
  [Keccak256] ->
  m Value
buildReceiptResponse blocks requestedHashes = buildReceiptResponseWithErrors blocks requestedHashes []

buildReceiptResponseWithErrors ::
  ( Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  [Block] ->
  [Keccak256] ->
  [Value] ->
  m Value
buildReceiptResponseWithErrors blocks requestedHashes initialErrors = do
  let txInfos = transactionInfoMap blocks
      blockHashes = fmap blockHash blocks
      allHashes = allTransactionHashes blocks
  txResults <- selectMany (Proxy @[TransactionResult]) allHashes
  rawLogs <- Logs.fetchLogs Logs.LogLookup
    { Logs.logLookupBlockHashes = nonEmpty blockHashes
    , Logs.logLookupTransactionHashes = Nothing
    , Logs.logLookupAddresses = Nothing
    }
  let enrichedLogs = Logs.annotateLogs blocks rawLogs
      logsByTransaction = M.fromListWith (++)
        [ (Logs.enrichedLogTransactionHash enrichedLog, [Logs.enrichedLogValue enrichedLog])
        | enrichedLog <- enrichedLogs
        ]
      cumulativeGasByTransaction = cumulativeGasMap blocks txResults
      (receipts, receiptErrors) = buildReceipts requestedHashes txInfos txResults logsByTransaction cumulativeGasByTransaction
  pure $ Mapper.receiptCollectionValue receipts (fmap Logs.enrichedLogValue enrichedLogs) (initialErrors ++ receiptErrors)

buildReceipts ::
  [Keccak256] ->
  M.Map Keccak256 (Mapper.TxContext, Transaction) ->
  M.Map Keccak256 [TransactionResult] ->
  M.Map Keccak256 [Value] ->
  M.Map Keccak256 Integer ->
  ([Value], [Value])
buildReceipts requestedHashes txInfos txResults logsByTransaction cumulativeGasByTransaction =
  foldl build ([], []) requestedHashes
  where
    build (receiptsAcc, errorsAcc) txHash =
      case (M.lookup txHash txInfos, listToMaybe =<< M.lookup txHash txResults) of
        (Just (context, tx), Just txResult) ->
          let cumulativeGasUsed = M.findWithDefault (toInteger $ transactionResultGasUsed txResult) txHash cumulativeGasByTransaction
              receipt = Mapper.receiptValue context tx txResult cumulativeGasUsed (M.findWithDefault [] txHash logsByTransaction)
           in (receiptsAcc ++ [receipt], errorsAcc)
        (Nothing, _) ->
          (receiptsAcc, errorsAcc ++ [Mapper.errorItem $ "missing transaction context for 0x" <> textHex txHash])
        (_, Nothing) ->
          (receiptsAcc, errorsAcc ++ [Mapper.errorItem $ "missing transaction result for 0x" <> textHex txHash])

transactionInfoMap :: [Block] -> M.Map Keccak256 (Mapper.TxContext, Transaction)
transactionInfoMap blocks =
  M.fromList $ concatMap blockEntries blocks
  where
    blockEntries block@Block{blockBlockData, blockReceiptTransactions} =
      [ ( transactionHash tx
        , ( Mapper.TxContext
              { Mapper.txContextBlockHash = blockHash block
              , Mapper.txContextBlockNumber = number blockBlockData
              , Mapper.txContextIndex = index
              }
          , tx
          )
        )
      | (index, tx) <- zip [0 ..] blockReceiptTransactions
      ]

cumulativeGasMap :: [Block] -> M.Map Keccak256 [TransactionResult] -> M.Map Keccak256 Integer
cumulativeGasMap blocks txResults =
  M.fromList $ concatMap blockEntries blocks
  where
    blockEntries Block{blockReceiptTransactions} =
      snd $ foldl build (0 :: Integer, []) blockReceiptTransactions
      where
        build (cumulativeGas, acc) tx =
          case listToMaybe =<< M.lookup (transactionHash tx) txResults of
            Nothing -> (cumulativeGas, acc)
            Just txResult ->
              let nextCumulative = cumulativeGas + toInteger (transactionResultGasUsed txResult)
               in (nextCumulative, acc ++ [(transactionHash tx, nextCumulative)])

allTransactionHashes :: [Block] -> [Keccak256]
allTransactionHashes blocks =
  [ transactionHash tx
  | Block{blockReceiptTransactions} <- blocks
  , tx <- blockReceiptTransactions
  ]

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

maybeToList :: Maybe a -> [a]
maybeToList (Just value) = [value]
maybeToList Nothing = []

textHex :: Keccak256 -> T.Text
textHex = T.pack . keccak256ToHex
