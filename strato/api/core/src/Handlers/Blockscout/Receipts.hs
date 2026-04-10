{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Receipts
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API =
  (    "receipts" :> "by-block-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "receipts" :> "by-transaction-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  )

server :: Monad m => ServerT API m
server = by_block_number :<|> by_transaction_hash
  where
    not_implemented = object ["status" .= ("not_implemented" :: String)]
    by_block_number _ = pure not_implemented
    by_transaction_hash _ = pure not_implemented
