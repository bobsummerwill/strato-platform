{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.ChainInfo
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API = "chain-info" :> Get '[JSON] Value

server :: Monad m => ServerT API m
server = pure $ object ["status" .= ("not_implemented" :: String)]
