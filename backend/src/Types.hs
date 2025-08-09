{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data PaymentRequest = PaymentRequest
  { fromAccount :: Text
  , toAccount   :: Text
  , amount      :: Double
  } deriving (Show, Generic)

instance ToJSON PaymentRequest
instance FromJSON PaymentRequest

data PaymentStatus = Success | Failed deriving (Show, Generic)
instance ToJSON PaymentStatus
instance FromJSON PaymentStatus

data PaymentRecord = PaymentRecord
  { paymentId     :: Int
  , fromAcc       :: Text
  , toAcc         :: Text
  , amountPaid    :: Double
  , status        :: PaymentStatus
  , timestamp     :: UTCTime
  } deriving (Show, Generic)

instance ToJSON PaymentRecord
instance FromJSON PaymentRecord
