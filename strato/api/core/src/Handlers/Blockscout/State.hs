{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.State
  ( API
  , server
  ) where

import Blockchain.Data.DataDefs (AddressStateRef(..))
import Blockchain.Strato.Model.Address (Address)
import Blockchain.Model.JsonBlock (AddressStateRef'(..))
import Control.Monad.Change.Alter (Selectable)
import Data.Aeson (Value)
import Handlers.AccountInfo
  ( AccountsFilterParams(..)
  , accountsFilterParams
  , getAccount'
  )
import qualified Handlers.Blockscout.Mapper as Mapper
import Handlers.Blockscout.Types
import Servant

type API =
  (    "state" :> "balances" :> ReqBody '[JSON] StateRequest :> Post '[JSON] Value
  :<|> "state" :> "nonces" :> ReqBody '[JSON] StateRequest :> Post '[JSON] Value
  :<|> "state" :> "codes" :> ReqBody '[JSON] StateRequest :> Post '[JSON] Value
  )

server :: (Selectable AccountsFilterParams [AddressStateRef] m) => ServerT API m
server = balances :<|> nonces :<|> codes

balances :: (Selectable AccountsFilterParams [AddressStateRef] m) => StateRequest -> m Value
balances StateRequest{requests} = do
  values <- mapM balanceValue requests
  pure $ Mapper.stateCollectionValue "balances" values

nonces :: (Selectable AccountsFilterParams [AddressStateRef] m) => StateRequest -> m Value
nonces StateRequest{requests} = do
  values <- mapM nonceValue requests
  pure $ Mapper.stateCollectionValue "nonces" values

codes :: Monad m => StateRequest -> m Value
codes _ = pure $ Mapper.notImplementedCollection "codes"

balanceValue :: (Selectable AccountsFilterParams [AddressStateRef] m) => StateLookup -> m Value
balanceValue StateLookup{addressHash, blockNumber} = do
  account <- lookupAccount addressHash
  let balance = maybe 0 addressStateRefBalance account
  pure $ Mapper.stateItemValue addressHash blockNumber balance

nonceValue :: (Selectable AccountsFilterParams [AddressStateRef] m) => StateLookup -> m Value
nonceValue StateLookup{addressHash, blockNumber} = do
  account <- lookupAccount addressHash
  let nonce = maybe 0 addressStateRefNonce account
  pure $ Mapper.stateItemValue addressHash blockNumber nonce

lookupAccount :: (Selectable AccountsFilterParams [AddressStateRef] m) => Address -> m (Maybe AddressStateRef)
lookupAccount address = do
  accounts <- getAccount' accountsFilterParams{_qaAddress = Just address}
  pure $ case accounts of
    AddressStateRef' account : _ -> Just account
    [] -> Nothing
