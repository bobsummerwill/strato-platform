{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.ChainInfo
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block(..))
import Blockchain.Data.BlockHeader (BlockHeader(..))
import Blockchain.EthConf (ethConf, networkConfig)
import qualified Blockchain.EthConf.Model as Conf
import Data.Aeson (Value)
import Handlers.BlkLast (GetLastBlocks(..))
import qualified Handlers.Blockscout.Mapper as Mapper
import Servant

type API = "chain-info" :> Get '[JSON] Value

server :: (GetLastBlocks m, Monad m) => ServerT API m
server = do
  blocks <- getLastBlocks 1
  let headNumber = case blocks of
        [] -> 0
        Block{blockBlockData} : _ -> number blockBlockData
  pure $ Mapper.chainInfoValue (Conf.networkID $ networkConfig ethConf) headNumber
