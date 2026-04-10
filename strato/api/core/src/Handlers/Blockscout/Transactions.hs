{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Transactions
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (BlockHeader(..))
import Blockchain.Data.DataDefs (RawTransaction(..))
import Blockchain.Data.Transaction (transactionHash)
import Blockchain.Model.JsonBlock (bPrimeToB, rtPrimeToRt)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.Keccak256 (Keccak256)
import Control.Monad.Change.Alter (Selectable)
import Data.Aeson (Value, object, (.=))
import Data.List (findIndex)
import Data.Maybe (listToMaybe)
import Handlers.Block (BlocksFilterParams(..), blocksFilterParams, getBlockInfo')
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Handlers.Transaction (TxsFilterParams(..), getTransaction', txsFilterParams)
import Servant

type API =
  (    "transactions" :> "by-hash" :> ReqBody '[JSON] TransactionHashesRequest :> Post '[JSON] Value
  :<|> "transactions" :> "count" :> "by-block-number" :> ReqBody '[JSON] BlockNumbersRequest :> Post '[JSON] Value
  )

server :: (Selectable BlocksFilterParams [Block] m, Selectable TxsFilterParams [RawTransaction] m) => ServerT API m
server = byHash :<|> countByBlockNumber

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

fetchTransaction :: (Selectable BlocksFilterParams [Block] m, Selectable TxsFilterParams [RawTransaction] m) => Keccak256 -> m (Maybe Value)
fetchTransaction txHash = do
  txs <- map rtPrimeToRt <$> getTransaction' txsFilterParams{qtHash = Just txHash}
  case txs of
    [] -> pure Nothing
    rawTx : _ -> do
      context <- lookupContext rawTx
      pure $ Just $ Mapper.transactionValueFromRaw context rawTx

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
