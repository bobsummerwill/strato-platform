{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Blockscout.Mapper
  ( LogContext(..)
  , TxContext(..)
  , blockBatchValue
  , blockValue
  , chainInfoValue
  , errorItem
  , logValue
  , notImplementedCollection
  , receiptCollectionValue
  , receiptValue
  , stateCollectionValue
  , stateItemValue
  , transactionCountValue
  , transactionValueFromRaw
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader
  ( BlockHeader(..)
  , getBlockBeneficiary
  , getBlockDifficulty
  , getBlockGasLimit
  , getBlockGasUsed
  , getBlockNonce
  , headerHash
  )
import Blockchain.Data.DataDefs (LogDB(..), RawTransaction(..), TransactionResult(..))
import Blockchain.Data.Transaction
  ( Transaction(..)
  , transactionHash
  , whoSignedThisTransaction
  )
import qualified Blockchain.Data.TransactionResultStatus as TRS
import Blockchain.Strato.Model.Address (Address)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.ExtendedWord (Word256)
import Blockchain.Strato.Model.Keccak256 (Keccak256, keccak256ToHex)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Text as T
import Text.Format (format)
import Numeric (showHex)

data TxContext = TxContext
  { txContextBlockHash :: Keccak256
  , txContextBlockNumber :: Integer
  , txContextIndex :: Int
  }
  deriving (Eq, Show)

data LogContext = LogContext
  { logContextBlockHash :: Keccak256
  , logContextBlockNumber :: Integer
  , logContextTransactionHash :: Keccak256
  , logContextTransactionIndex :: Int
  , logContextIndex :: Int
  }
  deriving (Eq, Show)

chainInfoValue :: Integer -> Integer -> Value
chainInfoValue chainId headNumber =
  object
    [ "chain_id" .= chainId
    , "head" .= headNumber
    , "safe_head" .= headNumber
    ]

blockBatchValue :: Bool -> [Block] -> Value
blockBatchValue hydrated blocks =
  let sortedBlocks = sortOn (number . blockBlockData) blocks
      detachedTransactions =
        if hydrated
          then []
          else concatMap detachedTransactionValues sortedBlocks
   in object
        [ "blocks" .= fmap (blockValue hydrated) sortedBlocks
        , "transactions" .= detachedTransactions
        , "errors" .= ([] :: [Value])
        ]

blockValue :: Bool -> Block -> Value
blockValue hydrated blk@Block{blockBlockData, blockReceiptTransactions, blockBlockUncles} =
  let blockNumber = number blockBlockData
      transactions =
        if hydrated
          then zipWith
                 (\index tx ->
                    transactionValueFromTransaction
                      (TxContext (blockHash blk) blockNumber index)
                      tx
                 )
                 [0 ..]
                 blockReceiptTransactions
          else []
   in object
        [ "hash" .= hexKeccak (blockHash blk)
        , "number" .= blockNumber
        , "parent_hash" .= hexKeccak (parentHash blockBlockData)
        , "timestamp" .= timestamp blockBlockData
        , "miner_hash" .= hexAddress (getBlockBeneficiary blockBlockData)
        , "gas_limit" .= getBlockGasLimit blockBlockData
        , "gas_used" .= getBlockGasUsed blockBlockData
        , "size" .= Null
        , "nonce" .= toInteger (getBlockNonce blockBlockData)
        , "difficulty" .= getBlockDifficulty blockBlockData
        , "total_difficulty" .= getBlockDifficulty blockBlockData
        , "base_fee_per_gas" .= (0 :: Integer)
        , "transactions" .= transactions
        , "uncles" .= fmap uncleValue blockBlockUncles
        , "withdrawals" .= ([] :: [Value])
        ]

transactionValueFromRaw :: Maybe TxContext -> RawTransaction -> Value
transactionValueFromRaw mCtx RawTransaction{..} =
  object
    [ "hash" .= hexKeccak rawTransactionTxHash
    , "block_hash" .= fmap (hexKeccak . txContextBlockHash) mCtx
    , "block_number" .= blockNumberFromContext mCtx (toInteger rawTransactionBlockNumber)
    , "index" .= fmap txContextIndex mCtx
    , "from_address_hash" .= hexAddress rawTransactionFromAddress
    , "to_address_hash" .= fmap hexAddress rawTransactionToAddress
    , "created_contract_address_hash" .= (Nothing :: Maybe T.Text)
    , "value" .= rawTransactionValue
    , "gas" .= rawTransactionGasLimit
    , "gas_price" .= rawTransactionGasPrice
    , "max_fee_per_gas" .= rawTransactionGasPrice
    , "max_priority_fee_per_gas" .= rawTransactionGasPrice
    , "input" .= ("0x" :: T.Text)
    , "nonce" .= rawTransactionNonce
    , "type" .= transactionTypeFromRaw rawTransactionToAddress rawTransactionContractName
    , "status" .= (Nothing :: Maybe T.Text)
    ]

transactionCountValue :: [(Integer, Int)] -> Value
transactionCountValue counts =
  object
    [ "transactions_count_map" .= Object (KeyMap.fromList [ (Key.fromString (show blockNumber), toJSON count) | (blockNumber, count) <- counts ])
    , "errors" .= ([] :: [Value])
    ]

stateCollectionValue :: T.Text -> [Value] -> Value
stateCollectionValue key values =
  Object $ KeyMap.fromList
    [ (Key.fromText key, toJSON values)
    , ("errors", toJSON ([] :: [Value]))
    ]

stateItemValue :: Address -> Integer -> Integer -> Value
stateItemValue address requestedBlock value =
  object
    [ "address_hash" .= hexAddress address
    , "block_number" .= requestedBlock
    , "value" .= value
    ]

receiptCollectionValue :: [Value] -> [Value] -> [Value] -> Value
receiptCollectionValue receipts logs errors =
  object
    [ "receipts" .= receipts
    , "logs" .= logs
    , "errors" .= errors
    ]

receiptValue :: TxContext -> Transaction -> TransactionResult -> Integer -> [Value] -> Value
receiptValue TxContext{..} tx txResult cumulativeGasUsed logs =
  object
    [ "transaction_hash" .= hexKeccak (transactionResultTransactionHash txResult)
    , "transaction_index" .= txContextIndex
    , "block_hash" .= hexKeccak txContextBlockHash
    , "block_number" .= txContextBlockNumber
    , "cumulative_gas_used" .= cumulativeGasUsed
    , "gas_used" .= gasUsed
    , "gas_price" .= transactionGasPrice tx
    , "created_contract_address_hash" .= fmap hexAddress (listToMaybe $ transactionResultContractsCreated txResult)
    , "status" .= receiptStatus txResult
    , "logs" .= logs
    , "logs_bloom" .= zeroLogsBloom
    ]
  where
    gasUsed = toInteger (transactionResultGasUsed txResult)

logValue :: LogContext -> LogDB -> Value
logValue LogContext{..} LogDB{logDBAddress, logDBTopic1, logDBTopic2, logDBTopic3, logDBTopic4, logDBTheData} =
  object
    [ "address_hash" .= hexAddress logDBAddress
    , "topics" .= catMaybes (map (fmap hexWord256) [logDBTopic1, logDBTopic2, logDBTopic3, logDBTopic4])
    , "data" .= hexBytes logDBTheData
    , "block_hash" .= hexKeccak logContextBlockHash
    , "block_number" .= logContextBlockNumber
    , "transaction_hash" .= hexKeccak logContextTransactionHash
    , "transaction_index" .= logContextTransactionIndex
    , "index" .= logContextIndex
    ]

notImplementedCollection :: T.Text -> Value
notImplementedCollection key =
  Object $ KeyMap.fromList
    [ (Key.fromText key, toJSON ([] :: [Value]))
    , ("errors", toJSON [errorItem (key <> " not implemented")])
    ]

errorItem :: T.Text -> Value
errorItem reason = object ["reason" .= reason]

transactionValueFromTransaction :: TxContext -> Transaction -> Value
transactionValueFromTransaction TxContext{..} tx =
  let txHashHex = hexKeccak $ transactionHash tx
      fromAddress = fmap hexAddress $ whoSignedThisTransaction tx
      makeTx :: Integer -> Integer -> Integer -> Integer -> Maybe T.Text -> Value
      makeTx nonce' gas' gasPrice' value' toAddress =
        object
          [ "hash" .= txHashHex
          , "block_hash" .= hexKeccak txContextBlockHash
          , "block_number" .= txContextBlockNumber
          , "index" .= txContextIndex
          , "from_address_hash" .= fromAddress
          , "to_address_hash" .= toAddress
          , "created_contract_address_hash" .= (Nothing :: Maybe T.Text)
          , "value" .= value'
          , "gas" .= gas'
          , "gas_price" .= gasPrice'
          , "max_fee_per_gas" .= gasPrice'
          , "max_priority_fee_per_gas" .= gasPrice'
          , "input" .= ("0x" :: T.Text)
          , "nonce" .= nonce'
          , "type" .= (0 :: Int)
          , "status" .= (Nothing :: Maybe T.Text)
          ]
   in case tx of
        EthereumTX{nonce, gasLimit, gasPrice, ethTo, value} ->
          makeTx nonce gasLimit gasPrice value (fmap hexAddress ethTo)
        MessageTX{nonce, gasLimit, to} ->
          makeTx nonce gasLimit (0 :: Integer) (0 :: Integer) (Just $ hexAddress to)
        ContractCreationTX{nonce, gasLimit} ->
          makeTx nonce gasLimit (0 :: Integer) (0 :: Integer) (Nothing :: Maybe T.Text)

uncleValue :: BlockHeader -> Value
uncleValue header =
  object
    [ "hash" .= hexKeccak (headerHash header)
    , "number" .= number header
    , "parent_hash" .= hexKeccak (parentHash header)
    ]

blockNumberFromContext :: Maybe TxContext -> Integer -> Maybe Integer
blockNumberFromContext (Just TxContext{txContextBlockNumber}) _ = Just txContextBlockNumber
blockNumberFromContext Nothing blockNumber
  | blockNumber < 0 = Nothing
  | otherwise = Just blockNumber

detachedTransactionValues :: Block -> [Value]
detachedTransactionValues blk@Block{blockBlockData, blockReceiptTransactions} =
  zipWith
    (\index tx ->
       transactionValueFromTransaction
         (TxContext (blockHash blk) (number blockBlockData) index)
         tx
    )
    [0 ..]
    blockReceiptTransactions

hexAddress :: Address -> T.Text
hexAddress address = T.pack $ "0x" ++ show address

hexKeccak :: Keccak256 -> T.Text
hexKeccak hashValue = T.pack $ "0x" ++ keccak256ToHex hashValue

hexWord256 :: Word256 -> T.Text
hexWord256 wordValue = T.pack $ "0x" ++ showHex wordValue ""

hexBytes :: BS.ByteString -> T.Text
hexBytes bytes = T.pack $ "0x" ++ format bytes

transactionTypeFromRaw :: Maybe Address -> Maybe T.Text -> Int
transactionTypeFromRaw Nothing _ = 0
transactionTypeFromRaw _ (Just _) = 0
transactionTypeFromRaw _ Nothing = 0

transactionGasPrice :: Transaction -> Integer
transactionGasPrice EthereumTX{gasPrice} = gasPrice
transactionGasPrice MessageTX{} = 0
transactionGasPrice ContractCreationTX{} = 0

receiptStatus :: TransactionResult -> T.Text
receiptStatus txResult
  | transactionResultStatus txResult == Just TRS.Success = "success"
  | otherwise = "failure"

zeroLogsBloom :: T.Text
zeroLogsBloom = "0x" <> T.replicate 512 "0"
