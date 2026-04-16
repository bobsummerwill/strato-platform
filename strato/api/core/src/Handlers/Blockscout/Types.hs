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
import Blockchain.Strato.Model.Keccak256 (Keccak256, stringKeccak256)
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.OpenApi
import qualified Data.Text as T
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
      <$> (o .: "hashes" >>= mapM (withText "Keccak256" (either fail pure . parseKeccak256Text)))
      <*> o .:? "hydrated"

instance FromJSON TransactionHashRequest where
  parseJSON = withObject "TransactionHashRequest" $ \o ->
    TransactionHashRequest <$> (o .: "hash" >>= withText "Keccak256" (either fail pure . parseKeccak256Text))

instance FromJSON TransactionHashesRequest where
  parseJSON = withObject "TransactionHashesRequest" $ \o ->
    TransactionHashesRequest <$> (o .: "hashes" >>= mapM (withText "Keccak256" (either fail pure . parseKeccak256Text)))

instance FromJSON FirstTraceLookup where
  parseJSON = withObject "FirstTraceLookup" $ \o ->
    FirstTraceLookup
      <$> (o .:? "block_hash" >>= traverse (withText "Keccak256" (either fail pure . parseKeccak256Text)))
      <*> o .: "block_number"
      <*> (o .: "hash_data" >>= withText "Keccak256" (either fail pure . parseKeccak256Text))
      <*> o .: "transaction_index"

instance FromJSON StateLookup where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON StateRequest where
  parseJSON = genericParseJSON jsonOptions

instance FromJSON LogSearchRequest where
  parseJSON = withObject "LogSearchRequest" $ \o ->
    LogSearchRequest
      <$> o .:? "from_block"
      <*> o .:? "to_block"
      <*> (o .:? "block_hash" >>= traverse (withText "Keccak256" (either fail pure . parseKeccak256Text)))
      <*> (o .:? "address" >>= traverse parseAddressField)
      <*> o .:? "topics"
    where
      parseAddressField addressValue =
        (parseJSON addressValue)
          <|> fmap pure (parseJSON addressValue)

instance ToSchema RangeRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema BlockNumbersRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema BlockHashesRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema TransactionHashRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema TransactionHashesRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema FirstTraceLookup where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema StateLookup where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema StateRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

instance ToSchema LogSearchRequest where
  declareNamedSchema proxy =
    genericDeclareNamedSchema blockscoutSchemaOptions proxy

hydratedOrTrue :: Maybe Bool -> Bool
hydratedOrTrue = fromMaybe True

jsonOptions :: Options
jsonOptions = defaultOptions {Data.Aeson.fieldLabelModifier = camelTo2 '_'}

parseKeccak256Text :: T.Text -> Either String Keccak256
parseKeccak256Text textValue =
  case stringKeccak256 (stripHexPrefix textValue) of
    Just hashValue -> Right hashValue
    Nothing -> Left $ "error parsing Keccak256: " ++ show textValue

stripHexPrefix :: T.Text -> String
stripHexPrefix textValue =
  case T.stripPrefix "0x" textValue <|> T.stripPrefix "0X" textValue of
    Just stripped -> T.unpack stripped
    Nothing -> T.unpack textValue

blockscoutSchemaOptions :: SchemaOptions
blockscoutSchemaOptions =
  defaultSchemaOptions {Data.OpenApi.fieldLabelModifier = camelTo2 '_'}
