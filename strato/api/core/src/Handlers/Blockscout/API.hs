{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.API
  ( API
  ) where

import Data.Aeson (Value)
import Servant

-- This is an initial namespace scaffold for the private Blockscout-oriented API.
-- It is intentionally not wired into Core.API yet. The concrete request and
-- response payloads are documented in BLOCKSCOUT_PRIVATE_API_SPEC.md.

type API =
  "blockscout" :>
    (    "chain-info" :> Get '[JSON] Value
    :<|> "blocks" :> "by-tag" :> Capture "tag" String :> QueryParam "hydrated" Bool :> Get '[JSON] Value
    :<|> "blocks" :> "range" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "blocks" :> "by-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "blocks" :> "by-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "transactions" :> "by-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "transactions" :> "count" :> "by-block-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "receipts" :> "by-block-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "receipts" :> "by-transaction-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "logs" :> "search" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "state" :> "balances" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "state" :> "nonces" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    :<|> "state" :> "codes" :> ReqBody '[JSON] Value :> Post '[JSON] Value
    )
