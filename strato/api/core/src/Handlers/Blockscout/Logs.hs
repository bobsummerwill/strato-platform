{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Handlers.Blockscout.Logs
  ( API
  , EnrichedLog(..)
  , LogLookup(..)
  , annotateLogs
  , fetchLogs
  , server
  ) where

import Blockchain.DB.SQLDB
import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (number)
import Blockchain.Data.DataDefs
import Blockchain.Data.Transaction (transactionHash)
import Blockchain.Strato.Model.Address (Address)
import Blockchain.Strato.Model.Class (blockHash)
import Blockchain.Strato.Model.ExtendedWord (Word256)
import Blockchain.Strato.Model.Keccak256 (Keccak256)
import Control.Monad (unless)
import Control.Monad.Change.Alter
import Control.Monad.Composable.SQL
import Data.Aeson (Value, object, (.=))
import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Database.Esqueleto.Legacy as E
import Handlers.Block (BlocksFilterParams)
import qualified Handlers.Blockscout.Blocks as Blocks
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Servant
import UnliftIO

type API = "logs" :> "search" :> ReqBody '[JSON] LogSearchRequest :> Post '[JSON] Value

data LogLookup = LogLookup
  { logLookupBlockHashes :: Maybe [Keccak256]
  , logLookupTransactionHashes :: Maybe [Keccak256]
  , logLookupAddresses :: Maybe [Address]
  }
  deriving (Eq, Ord, Show)

data EnrichedLog = EnrichedLog
  { enrichedLogBlockHash :: Keccak256
  , enrichedLogTransactionHash :: Keccak256
  , enrichedLogValue :: Value
  }

instance {-# OVERLAPPING #-} MonadUnliftIO m => Selectable LogLookup [LogDB] (SQLM m) where
  select _ lookupParams = fmap (Just . map E.entityVal) . sqlQuery $
    E.select $
      E.from $ \logEntry -> do
        let criteria = catMaybes
              [ fmap (\hashes -> (logEntry E.^. LogDBBlockHash) `E.in_` E.valList hashes) (nonEmpty $ logLookupBlockHashes lookupParams)
              , fmap (\hashes -> (logEntry E.^. LogDBTransactionHash) `E.in_` E.valList hashes) (nonEmpty $ logLookupTransactionHashes lookupParams)
              , fmap (\addresses -> (logEntry E.^. LogDBAddress) `E.in_` E.valList addresses) (nonEmpty $ logLookupAddresses lookupParams)
              ]

        unless (null criteria) $ E.where_ (foldl1 (E.&&.) criteria)
        return logEntry

server :: (Selectable LogLookup [LogDB] m, Selectable BlocksFilterParams [Block] m) => ServerT API m
server request = do
  blocks <- resolveBlocks request
  let blockHashes = fmap blockHash blocks
  rawLogs <- fetchLogs LogLookup
    { logLookupBlockHashes = nonEmpty $ Just blockHashes
    , logLookupTransactionHashes = Nothing
    , logLookupAddresses = requestedLogAddresses request
    }
  let enrichedLogs = annotateLogs blocks $ filter (matchesTopics $ requestedLogTopics request) rawLogs
  pure $ object
    [ "logs" .= fmap enrichedLogValue enrichedLogs
    , "errors" .= ([] :: [Value])
    ]

fetchLogs :: (Selectable LogLookup [LogDB] m) => LogLookup -> m [LogDB]
fetchLogs lookupParams = fromMaybe [] <$> select (Proxy @[LogDB]) lookupParams

annotateLogs :: [Block] -> [LogDB] -> [EnrichedLog]
annotateLogs blocks rawLogs =
  snd $ foldl attach (M.empty, []) sortedLogs
  where
    contexts = transactionContextMap blocks
    sortedLogs = sortOn sortKey $ filter (\logEntry -> M.member (logDBTransactionHash logEntry) contexts) rawLogs

    sortKey logEntry =
      case M.lookup (logDBTransactionHash logEntry) contexts of
        Just context -> (Mapper.txContextBlockNumber context, Mapper.txContextIndex context)
        Nothing -> (0, 0)

    attach (blockCounters, acc) logEntry =
      case M.lookup (logDBTransactionHash logEntry) contexts of
        Nothing -> (blockCounters, acc)
        Just context ->
          let blockCounter = M.findWithDefault 0 (Mapper.txContextBlockHash context) blockCounters
              logContext = Mapper.LogContext
                { Mapper.logContextBlockHash = Mapper.txContextBlockHash context
                , Mapper.logContextBlockNumber = Mapper.txContextBlockNumber context
                , Mapper.logContextTransactionHash = logDBTransactionHash logEntry
                , Mapper.logContextTransactionIndex = Mapper.txContextIndex context
                , Mapper.logContextIndex = blockCounter
                }
              enriched = EnrichedLog
                { enrichedLogBlockHash = Mapper.txContextBlockHash context
                , enrichedLogTransactionHash = logDBTransactionHash logEntry
                , enrichedLogValue = Mapper.logValue logContext logEntry
                }
           in ( M.insert (Mapper.txContextBlockHash context) (blockCounter + 1) blockCounters
              , acc ++ [enriched]
              )

resolveBlocks :: (Selectable BlocksFilterParams [Block] m) => LogSearchRequest -> m [Block]
resolveBlocks LogSearchRequest{requestedLogBlockHash = Just blockHashValue} = do
  maybeBlock <- Blocks.fetchBlockByHash blockHashValue
  pure $ maybe [] pure maybeBlock
resolveBlocks LogSearchRequest{requestedLogFromBlock, requestedLogToBlock} =
  case (requestedLogFromBlock, requestedLogToBlock) of
    (Just fromBlock, Just toBlock) -> Blocks.fetchBlocksInRange fromBlock toBlock
    (Just blockNumber, Nothing) -> Blocks.fetchBlocksInRange blockNumber blockNumber
    (Nothing, Just blockNumber) -> Blocks.fetchBlocksInRange blockNumber blockNumber
    (Nothing, Nothing) -> pure []

transactionContextMap :: [Block] -> M.Map Keccak256 Mapper.TxContext
transactionContextMap blocks =
  M.fromList $ do
    block@Block{blockBlockData, blockReceiptTransactions} <- blocks
    (index, tx) <- zip [0 ..] blockReceiptTransactions
    pure
      ( transactionHash tx
      , Mapper.TxContext
          { Mapper.txContextBlockHash = blockHash block
          , Mapper.txContextBlockNumber = number blockBlockData
          , Mapper.txContextIndex = index
          }
      )

matchesTopics :: Maybe [Maybe [Word256]] -> LogDB -> Bool
matchesTopics Nothing _ = True
matchesTopics (Just requestedTopics) logEntry = and $ zipWith matchesTopic ([0 .. 3] :: [Int]) requestedTopics
  where
    matchesTopic _ Nothing = True
    matchesTopic position (Just acceptableTopics) =
      case topicAt position logEntry of
        Nothing -> False
        Just topicValue -> topicValue `elem` acceptableTopics

    topicAt 0 = logDBTopic1
    topicAt 1 = logDBTopic2
    topicAt 2 = logDBTopic3
    topicAt 3 = logDBTopic4
    topicAt _ = const Nothing

nonEmpty :: Maybe [a] -> Maybe [a]
nonEmpty (Just []) = Nothing
nonEmpty other = other
