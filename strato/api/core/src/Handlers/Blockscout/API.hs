{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.API
  ( API
  , server
  ) where

import qualified Handlers.Blockscout.Blocks as Blocks
import qualified Handlers.Blockscout.ChainInfo as ChainInfo
import qualified Handlers.Blockscout.Logs as Logs
import qualified Handlers.Blockscout.Receipts as Receipts
import qualified Handlers.Blockscout.State as State
import qualified Handlers.Blockscout.Transactions as Transactions
import Servant

-- This is an initial namespace scaffold for the private Blockscout-oriented API.
-- It is intentionally not wired into Core.API yet. The concrete request and
-- response payloads are documented in BLOCKSCOUT_PRIVATE_API_SPEC.md.

type API =
  "blockscout" :>
    (    ChainInfo.API
    :<|> Blocks.API
    :<|> Transactions.API
    :<|> Receipts.API
    :<|> Logs.API
    :<|> State.API
    )

server :: Monad m => ServerT API m
server =
  ChainInfo.server
    :<|> Blocks.server
    :<|> Transactions.server
    :<|> Receipts.server
    :<|> Logs.server
    :<|> State.server
