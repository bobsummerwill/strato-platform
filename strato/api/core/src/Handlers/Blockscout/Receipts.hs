{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Receipts
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (number)
import Blockchain.Data.Transaction (Transaction(..), transactionHash)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.Keccak256 (Keccak256, keccak256ToHex)
import Control.Monad.Change.Alter (Selectable)
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Handlers.Block (BlocksFilterParams)
import qualified Handlers.Blockscout.Blocks as Blocks
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Servant

type API =
  (    "receipts" :> "by-block-number" :> ReqBody '[JSON] BlockNumbersRequest :> Post '[JSON] Value
  :<|> "receipts" :> "by-transaction-hash" :> ReqBody '[JSON] TransactionHashesRequest :> Post '[JSON] Value
  )

server :: Selectable BlocksFilterParams [Block] m => ServerT API m
server = byBlockNumber :<|> byTransactionHash

byBlockNumber :: Selectable BlocksFilterParams [Block] m => BlockNumbersRequest -> m Value
byBlockNumber BlockNumbersRequest{requestedBlockNumbers} = do
  blocks <- catMaybes <$> mapM Blocks.fetchBlockByNumber requestedBlockNumbers
  pure $ buildReceiptResponse blocks (allTransactionHashes blocks) []

byTransactionHash :: Selectable BlocksFilterParams [Block] m => TransactionHashesRequest -> m Value
byTransactionHash TransactionHashesRequest{requestedTransactionHashes} = do
  blocks <- Blocks.fetchBlocksInRange 0 20000
  let requestedBlocks =
        [ block
        | block <- blocks
        , any (`elem` requestedTransactionHashes) [transactionHash tx | tx <- blockReceiptTransactions block]
        ]
      presentHashes = allTransactionHashes requestedBlocks
      missingErrors =
        [ Mapper.errorItem $ "missing transaction context for 0x" <> textHex txHash
        | txHash <- requestedTransactionHashes
        , txHash `notElem` presentHashes
        ]
  pure $ buildReceiptResponse requestedBlocks requestedTransactionHashes missingErrors

buildReceiptResponse :: [Block] -> [Keccak256] -> [Value] -> Value
buildReceiptResponse blocks requestedHashes initialErrors =
  let txInfos = transactionInfoMap blocks
      (receipts, receiptErrors) = buildReceipts requestedHashes txInfos
   in Mapper.receiptCollectionValue receipts [] (initialErrors ++ receiptErrors)

buildReceipts ::
  [Keccak256] ->
  M.Map Keccak256 (Mapper.TxContext, Transaction) ->
  ([Value], [Value])
buildReceipts requestedHashes txInfos =
  foldl build ([], []) requestedHashes
  where
    build (receiptsAcc, errorsAcc) txHash =
      case M.lookup txHash txInfos of
        Just (context, tx) ->
          (receiptsAcc ++ [syntheticReceiptValue context tx], errorsAcc)

        Nothing ->
          (receiptsAcc, errorsAcc ++ [Mapper.errorItem $ "missing transaction context for 0x" <> textHex txHash])

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

allTransactionHashes :: [Block] -> [Keccak256]
allTransactionHashes blocks =
  [ transactionHash tx
  | Block{blockReceiptTransactions} <- blocks
  , tx <- blockReceiptTransactions
  ]

textHex :: Keccak256 -> T.Text
textHex = T.pack . keccak256ToHex

syntheticReceiptValue :: Mapper.TxContext -> Transaction -> Value
syntheticReceiptValue Mapper.TxContext{..} tx =
  object
    [ "transaction_hash" .= T.pack ("0x" ++ keccak256ToHex (transactionHash tx))
    , "transaction_index" .= txContextIndex
    , "block_hash" .= T.pack ("0x" ++ keccak256ToHex txContextBlockHash)
    , "block_number" .= txContextBlockNumber
    , "cumulative_gas_used" .= (0 :: Integer)
    , "gas_used" .= (0 :: Integer)
    , "gas_price" .= syntheticGasPrice tx
    , "created_contract_address_hash" .= (Nothing :: Maybe T.Text)
    , "status" .= ("success" :: T.Text)
    , "logs" .= ([] :: [Value])
    , "logs_bloom" .= ("0x" <> T.replicate 512 "0")
    ]

syntheticGasPrice :: Transaction -> Integer
syntheticGasPrice EthereumTX{gasPrice} = gasPrice
syntheticGasPrice MessageTX{} = 0
syntheticGasPrice ContractCreationTX{} = 0
