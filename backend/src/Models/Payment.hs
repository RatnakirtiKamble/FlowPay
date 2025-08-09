-- src/Models/Payment.hs
{-# LANGUAGE DeriveGeneric #-}

module Models.Payment where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Database.PostgreSQL.Simple.ToField (ToField(toField)) -- Corrected this line

-- | The status of a payment.
data PaymentStatus = Success | Failed
  deriving (Show, Generic)

-- Make it compatible with JSON and the DB (as Text)
instance ToJSON PaymentStatus
instance FromJSON PaymentStatus
instance ToField PaymentStatus where
  -- This implementation is now correct because `toField` is in scope.
  toField = toField . show

-- | The request body for a payment.
data PaymentRequest = PaymentRequest
  { fromAccount :: Text
  , toAccount   :: Text
  , amount      :: Double
  } deriving (Show, Generic)

instance FromJSON PaymentRequest
instance ToJSON PaymentRequest

-- | A full payment record as stored in the database.
data Payment = Payment
  { paymentId :: Int
  , fromAcc   :: Text
  , toAcc     :: Text
  , paid      :: Double
  , status    :: Text
  , timestamp :: UTCTime
  } deriving (Show, Generic)

instance ToJSON Payment
instance FromRow Payment

-- | The successful response after a payment is made.
type PaymentResponse = Payment