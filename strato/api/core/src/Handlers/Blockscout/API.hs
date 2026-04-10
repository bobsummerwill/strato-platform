{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Blockscout.API
  ( API
  , server
  ) where

import Blockchain.Data.Block (Block)
import Blockchain.Data.DataDefs (AddressStateRef, LogDB, RawTransaction, TransactionResult)
import Blockchain.Strato.Model.Keccak256 (Keccak256)
import Control.Monad.Change.Alter (Selectable)
import qualified Handlers.AccountInfo as AccountInfo
import qualified Handlers.BlkLast as BlkLast
import qualified Handlers.Block as Block
import qualified Handlers.Transaction as Transaction
import qualified Handlers.Blockscout.Blocks as Blocks
import qualified Handlers.Blockscout.ChainInfo as ChainInfo
import qualified Handlers.Blockscout.Logs as Logs
import qualified Handlers.Blockscout.Receipts as Receipts
import qualified Handlers.Blockscout.State as State
import qualified Handlers.Blockscout.Transactions as Transactions
import Servant

type API =
  "blockscout" :>
    (    ChainInfo.API
    :<|> Blocks.API
    :<|> Transactions.API
    :<|> Receipts.API
    :<|> Logs.API
    :<|> State.API
    )

server ::
  ( BlkLast.GetLastBlocks m
  , Selectable Block.BlocksFilterParams [Block] m
  , Selectable Transaction.TxsFilterParams [RawTransaction] m
  , Selectable AccountInfo.AccountsFilterParams [AddressStateRef] m
  , Selectable Keccak256 [TransactionResult] m
  , Selectable Logs.LogLookup [LogDB] m
  ) =>
  ServerT API m
server =
  ChainInfo.server
    :<|> Blocks.server
    :<|> Transactions.server
    :<|> Receipts.server
    :<|> Logs.server
    :<|> State.server
