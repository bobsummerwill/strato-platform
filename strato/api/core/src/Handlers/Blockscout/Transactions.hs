{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Transactions
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API =
  (    "transactions" :> "by-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "transactions" :> "count" :> "by-block-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  )

server :: Monad m => ServerT API m
server = by_hash :<|> count_by_block_number
  where
    not_implemented = object ["status" .= ("not_implemented" :: String)]
    by_hash _ = pure not_implemented
    count_by_block_number _ = pure not_implemented
