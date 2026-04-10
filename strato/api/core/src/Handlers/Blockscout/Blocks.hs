{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Blocks
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API =
  (    "blocks" :> "by-tag" :> Capture "tag" String :> QueryParam "hydrated" Bool :> Get '[JSON] Value
  :<|> "blocks" :> "range" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "blocks" :> "by-number" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "blocks" :> "by-hash" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  )

server :: Monad m => ServerT API m
server =
  by_tag :<|> by_range :<|> by_number :<|> by_hash
  where
    not_implemented = object ["status" .= ("not_implemented" :: String)]
    by_tag _ _ = pure not_implemented
    by_range _ = pure not_implemented
    by_number _ = pure not_implemented
    by_hash _ = pure not_implemented
