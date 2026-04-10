{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Blocks
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block)
import Blockchain.Model.JsonBlock (bPrimeToB)
import Blockchain.Strato.Model.Keccak256 (Keccak256)
import Control.Monad.Change.Alter (Selectable)
import Data.Aeson (Value, object, (.=))
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Handlers.BlkLast (GetLastBlocks(..))
import Handlers.Block (BlocksFilterParams(..), blocksFilterParams, getBlockInfo')
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Servant
import SortDirection (Sortby(ASC))

type API =
  (    "blocks" :> "by-tag" :> Capture "tag" String :> QueryParam "hydrated" Bool :> Get '[JSON] Value
  :<|> "blocks" :> "range" :> ReqBody '[JSON] RangeRequest :> Post '[JSON] Value
  :<|> "blocks" :> "by-number" :> ReqBody '[JSON] BlockNumbersRequest :> Post '[JSON] Value
  :<|> "blocks" :> "by-hash" :> ReqBody '[JSON] BlockHashesRequest :> Post '[JSON] Value
  )

server :: (GetLastBlocks m, Selectable BlocksFilterParams [Block] m) => ServerT API m
server = byTag :<|> byRange :<|> byNumber :<|> byHash

byTag :: (GetLastBlocks m, Selectable BlocksFilterParams [Block] m) => String -> Maybe Bool -> m Value
byTag tag hydrated = do
  block <- case tag of
    "latest" -> latestBlock
    "safe" -> latestBlock
    "pending" -> latestBlock
    "earliest" -> fetchBlockByNumber 0
    _ -> pure Nothing

  pure $ case block of
    Just blk -> Mapper.blockValue (hydratedOrTrue hydrated) blk
    Nothing -> object ["error" .= T.pack ("unsupported or missing block tag: " ++ tag)]

byRange :: (Selectable BlocksFilterParams [Block] m) => RangeRequest -> m Value
byRange RangeRequest{rangeFrom, rangeTo, rangeHydrated} = do
  blocks <- map bPrimeToB <$> getBlockInfo' blocksFilterParams
    { qbMinNumber = Just (fromIntegral rangeFrom)
    , qbMaxNumber = Just (fromIntegral rangeTo)
    , qbSortby = Just ASC
    }
  pure $ Mapper.blockBatchValue (hydratedOrTrue rangeHydrated) blocks

byNumber :: (Selectable BlocksFilterParams [Block] m) => BlockNumbersRequest -> m Value
byNumber BlockNumbersRequest{requestedBlockNumbers, blockNumbersHydrated} = do
  blocks <- mapMaybe id <$> mapM fetchBlockByNumber requestedBlockNumbers
  pure $ Mapper.blockBatchValue (hydratedOrTrue blockNumbersHydrated) blocks

byHash :: (Selectable BlocksFilterParams [Block] m) => BlockHashesRequest -> m Value
byHash BlockHashesRequest{requestedBlockHashes, blockHashesHydrated} = do
  blocks <- mapMaybe id <$> mapM fetchBlockByHash requestedBlockHashes
  pure $ Mapper.blockBatchValue (hydratedOrTrue blockHashesHydrated) blocks

latestBlock :: (GetLastBlocks m, Monad m) => m (Maybe Block)
latestBlock = do
  blocks <- getLastBlocks 1
  pure $ listToMaybe blocks

fetchBlockByNumber :: (Selectable BlocksFilterParams [Block] m) => Integer -> m (Maybe Block)
fetchBlockByNumber blockNumber = do
  blocks <- map bPrimeToB <$> getBlockInfo' blocksFilterParams{qbNumber = Just (fromIntegral blockNumber)}
  pure $ listToMaybe blocks

fetchBlockByHash :: (Selectable BlocksFilterParams [Block] m) => Keccak256 -> m (Maybe Block)
fetchBlockByHash blockHash = do
  blocks <- map bPrimeToB <$> getBlockInfo' blocksFilterParams{qbHash = Just blockHash}
  pure $ listToMaybe blocks
