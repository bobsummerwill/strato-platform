{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Blockscout.Types
  ( RangeRequest(..)
  , BlockNumbersRequest(..)
  , BlockHashesRequest(..)
  , TransactionHashRequest(..)
  , TransactionHashesRequest(..)
  , FirstTraceLookup(..)
  , StateLookup(..)
  , StateRequest(..)
  , LogSearchRequest(..)
  , hydratedOrTrue
  ) where

import Blockchain.Strato.Model.Address (Address)
import Blockchain.Strato.Model.ExtendedWord (Word256)
import Blockchain.Strato.Model.Keccak256 (Keccak256)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

data RangeRequest = RangeRequest
  { rangeFrom :: Integer
  , rangeTo :: Integer
  , rangeHydrated :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data BlockNumbersRequest = BlockNumbersRequest
  { requestedBlockNumbers :: [Integer]
  , blockNumbersHydrated :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data BlockHashesRequest = BlockHashesRequest
  { requestedBlockHashes :: [Keccak256]
  , blockHashesHydrated :: Maybe Bool
  }
  deriving (Eq, Show, Generic)

data TransactionHashRequest = TransactionHashRequest
  { requestedTransactionHash :: Keccak256
  }
  deriving (Eq, Show, Generic)

data TransactionHashesRequest = TransactionHashesRequest
  { requestedTransactionHashes :: [Keccak256]
  }
  deriving (Eq, Show, Generic)

data FirstTraceLookup = FirstTraceLookup
  { lookupBlockHash :: Maybe Keccak256
  , lookupBlockNumber :: Integer
  , lookupHashData :: Keccak256
  , lookupTransactionIndex :: Int
  }
  deriving (Eq, Show, Generic)

data StateLookup = StateLookup
  { addressHash :: Address
  , blockNumber :: Integer
  }
  deriving (Eq, Show, Generic)

data StateRequest = StateRequest
  { requests :: [StateLookup]
  }
  deriving (Eq, Show, Generic)

data LogSearchRequest = LogSearchRequest
  { requestedLogFromBlock :: Maybe Integer
  , requestedLogToBlock :: Maybe Integer
  , requestedLogBlockHash :: Maybe Keccak256
  , requestedLogAddresses :: Maybe [Address]
  , requestedLogTopics :: Maybe [Maybe [Word256]]
  }
  deriving (Eq, Show, Generic)

instance FromJSON RangeRequest where
  parseJSON = withObject "RangeRequest" $ \o ->
    RangeRequest
      <$> o .: "from"
      <*> o .: "to"
      <*> o .:? "hydrated"

instance FromJSON BlockNumbersRequest where
  parseJSON = withObject "BlockNumbersRequest" $ \o ->
    BlockNumbersRequest
      <$> o .: "block_numbers"
      <*> o .:? "hydrated"

instance FromJSON BlockHashesRequest where
  parseJSON = withObject "BlockHashesRequest" $ \o ->
    BlockHashesRequest
      <$> o .: "hashes"
      <*> o .:? "hydrated"

instance FromJSON TransactionHashRequest where
  parseJSON = withObject "TransactionHashRequest" $ \o ->
    TransactionHashRequest <$> o .: "hash"

instance FromJSON TransactionHashesRequest where
  parseJSON = withObject "TransactionHashesRequest" $ \o ->
    TransactionHashesRequest <$> o .: "hashes"

instance FromJSON FirstTraceLookup where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON StateLookup where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON StateRequest where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON LogSearchRequest where
  parseJSON = withObject "LogSearchRequest" $ \o ->
    LogSearchRequest
      <$> o .:? "from_block"
      <*> o .:? "to_block"
      <*> o .:? "block_hash"
      <*> (o .:? "address" >>= traverse parseAddressField)
      <*> o .:? "topics"
    where
      parseAddressField value =
        (parseJSON value)
          <|> fmap pure (parseJSON value)

hydratedOrTrue :: Maybe Bool -> Bool
hydratedOrTrue = fromMaybe True

jsonOptions :: Options
jsonOptions = defaultOptions {fieldLabelModifier = camelTo2 '_'}
