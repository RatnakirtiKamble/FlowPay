{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Merchant where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(Object))
import Data.Text (Text)
import Data.Maybe (isJust)
import Database.PostgreSQL.Simple (ToRow) 
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Servant.Auth.Server (ToJWT, FromJWT)

data Merchant = Merchant
  { merchantId           :: Int
  , merchantName         :: Text
  , merchantEmail        :: Text
  , merchantPasswordHash :: Text 
  , merchantApiKey       :: Maybe Text 
  , merchantBalance      :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON Merchant where
  toJSON merchant = object
    [ "merchantId"      .= merchantId merchant
    , "merchantName"    .= merchantName merchant
    , "merchantEmail"   .= merchantEmail merchant
    , "merchantBalance" .= merchantBalance merchant
    ]

instance FromJSON Merchant where
  parseJSON (Object v) = Merchant
    <$> v .: "merchantId"
    <*> v .: "merchantName"
    <*> v .: "merchantEmail"
    <*> pure ""    
    <*> pure Nothing
    <*> v .: "merchantBalance"
  parseJSON _ = mempty

instance ToJWT Merchant
instance FromJWT Merchant

instance FromRow Merchant where
  fromRow = Merchant <$> field <*> field <*> field <*> field <*> field <*> field
instance ToRow Merchant

data PublicMerchant = PublicMerchant
  { publicMerchantId      :: Int
  , publicMerchantName    :: Text
  , publicMerchantEmail   :: Text
  , publicMerchantBalance :: Double
  , publicMerchantApiKeyExists :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON PublicMerchant

toPublicMerchant :: Merchant -> PublicMerchant
toPublicMerchant merchant =
  PublicMerchant
    { publicMerchantId    = merchantId merchant
    , publicMerchantName  = merchantName merchant
    , publicMerchantEmail = merchantEmail merchant
    , publicMerchantBalance = merchantBalance merchant
    , publicMerchantApiKeyExists = isJust (merchantApiKey merchant)
    }

data RegistrationRequest = RegistrationRequest
  { reqName     :: Text
  , reqEmail    :: Text
  , reqPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RegistrationRequest

data LoginRequest = LoginRequest
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginRequest

data BalanceResponse = BalanceResponse
  { currentBalance :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON BalanceResponse
