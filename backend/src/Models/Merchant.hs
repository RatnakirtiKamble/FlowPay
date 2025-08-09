{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Merchant where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), (.:), Value(Object))
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Servant.Auth.Server (ToJWT, FromJWT)

data Merchant = Merchant
  { merchantId           :: Int
  , merchantName         :: Text
  , merchantEmail        :: Text
  , merchantPasswordHash :: Text 
  , merchantApiKey       :: Text 
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
    <*> pure ""    
    <*> v .: "merchantBalance"
  parseJSON _ = mempty

instance ToJWT Merchant
instance FromJWT Merchant

instance FromRow Merchant
instance ToRow Merchant

data PublicMerchant = PublicMerchant
  { publicMerchantId      :: Int
  , publicMerchantName    :: Text
  , publicMerchantEmail   :: Text
  , publicMerchantBalance :: Double
  } deriving (Eq, Show, Generic)

instance ToJSON PublicMerchant

toPublicMerchant :: Merchant -> PublicMerchant
toPublicMerchant m = PublicMerchant
  { publicMerchantId = merchantId m
  , publicMerchantName = merchantName m
  , publicMerchantEmail = merchantEmail m
  , publicMerchantBalance = merchantBalance m
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
