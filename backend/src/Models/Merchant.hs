{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Models.Merchant
Description : Data models for merchant entities, API requests, and responses.

This module defines:
  * Full merchant records from the database
  * Public merchant representation (no sensitive data)
  * Request/response types for authentication and balance queries
  * JSON serialization/deserialization
  * PostgreSQL row mapping
  * JWT instances for authentication
-}
module Models.Merchant where

-- ========== Imports ==========
import GHC.Generics (Generic)
import Data.Aeson
  ( ToJSON(..), FromJSON(..), object, (.=), (.:), (.!=), (.:?), Value(Object) )
import Data.Text (Text)
import Data.Maybe (isJust)
import Database.PostgreSQL.Simple (ToRow) 
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Servant.Auth.Server (ToJWT, FromJWT)

-- ========== Merchant Entity (Full DB Record) ==========

-- | Full merchant record as stored in the database, including sensitive fields.
data Merchant = Merchant
  { merchantId           :: Int          -- ^ Unique merchant ID
  , merchantName         :: Text         -- ^ Merchant's display name
  , merchantEmail        :: Text         -- ^ Merchant's email
  , merchantPasswordHash :: Text         -- ^ Hashed password
  , merchantApiKey       :: Maybe Text   -- ^ Optional API key
  , merchantBalance      :: Double       -- ^ Current balance
  } deriving (Eq, Show, Generic)

-- | Serialize merchant to JSON (omitting password hash).
instance ToJSON Merchant where
  toJSON merchant = object
    [ "merchantId"      .= merchantId merchant
    , "merchantName"    .= merchantName merchant
    , "merchantEmail"   .= merchantEmail merchant
    , "merchantBalance" .= merchantBalance merchant
    , "merchantApiKey"  .= merchantApiKey merchant 
    ]

-- | Parse merchant from JSON (password is not expected from API clients here).
instance FromJSON Merchant where
  parseJSON (Object v) = Merchant
    <$> v .: "merchantId"
    <*> v .: "merchantName"
    <*> v .: "merchantEmail"
    <*> pure "" -- Password hash not sent in JSON
    <*> v .:? "merchantApiKey" .!= Nothing
    <*> v .: "merchantBalance"
  parseJSON _ = mempty

-- | JWT instances for authentication.
instance ToJWT Merchant
instance FromJWT Merchant

-- | Database row mapping for merchant.
instance FromRow Merchant where
  fromRow = Merchant <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Merchant

-- ========== Public Merchant Representation ==========

-- | Public-facing merchant data (safe for client exposure).
data PublicMerchant = PublicMerchant
  { publicMerchantId          :: Int     -- ^ Merchant ID
  , publicMerchantName        :: Text    -- ^ Merchant name
  , publicMerchantEmail       :: Text    -- ^ Merchant email
  , publicMerchantBalance     :: Double  -- ^ Current balance
  , publicMerchantApiKeyExists :: Bool   -- ^ Whether API key exists
  } deriving (Eq, Show, Generic)

instance ToJSON PublicMerchant

-- | Convert full merchant record to public representation.
toPublicMerchant :: Merchant -> PublicMerchant
toPublicMerchant merchant =
  PublicMerchant
    { publicMerchantId          = merchantId merchant
    , publicMerchantName        = merchantName merchant
    , publicMerchantEmail       = merchantEmail merchant
    , publicMerchantBalance     = merchantBalance merchant
    , publicMerchantApiKeyExists = isJust (merchantApiKey merchant)
    }

-- ========== Request & Response Types ==========

-- | Registration request payload.
data RegistrationRequest = RegistrationRequest
  { reqName     :: Text -- ^ Merchant name
  , reqEmail    :: Text -- ^ Merchant email
  , reqPassword :: Text -- ^ Plaintext password
  } deriving (Eq, Show, Generic)
instance FromJSON RegistrationRequest

-- | Login request payload.
data LoginRequest = LoginRequest
  { loginEmail    :: Text -- ^ Merchant email
  , loginPassword :: Text -- ^ Plaintext password
  } deriving (Eq, Show, Generic)
instance FromJSON LoginRequest

-- | Login response payload containing JWT, XSRF token, and merchant details.
data LoginResponse = LoginResponse
  { lrAccessToken :: Text           -- ^ JWT access token
  , lrXsrfToken   :: Text           -- ^ XSRF token for CSRF protection
  , lrMerchant    :: PublicMerchant -- ^ Public merchant details
  } deriving (Generic, Show)
instance ToJSON LoginResponse

-- | Balance query response.
data BalanceResponse = BalanceResponse
  { currentBalance :: Double -- ^ Merchant's current balance
  } deriving (Eq, Show, Generic)
instance ToJSON BalanceResponse
