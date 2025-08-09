{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Payment where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField(toField))

data PaymentRequest = PaymentRequest
  { paymentAmount :: Double     
  } deriving (Show, Generic)

instance FromJSON PaymentRequest
instance ToJSON PaymentRequest

data PaymentResponse = PaymentResponse
  { paymentStatus :: Text
  , newBalance   :: Double
  } deriving (Show, Generic)

instance ToJSON PaymentResponse
instance FromJSON PaymentResponse
