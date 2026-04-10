{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.State
  ( API
  , server
  ) where

import Data.Aeson (Value, object, (.=))
import Servant

type API =
  (    "state" :> "balances" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "state" :> "nonces" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  :<|> "state" :> "codes" :> ReqBody '[JSON] Value :> Post '[JSON] Value
  )

server :: Monad m => ServerT API m
server = balances :<|> nonces :<|> codes
  where
    not_implemented = object ["status" .= ("not_implemented" :: String)]
    balances _ = pure not_implemented
    nonces _ = pure not_implemented
    codes _ = pure not_implemented
