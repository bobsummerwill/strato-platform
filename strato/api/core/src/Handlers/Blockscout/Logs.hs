{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.Logs
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API = "logs" :> "search" :> ReqBody '[JSON] Value :> Post '[JSON] Value

server :: Monad m => ServerT API m
server _ = pure $ object ["status" .= ("not_implemented" :: String)]
