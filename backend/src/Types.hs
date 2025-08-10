{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Types
-- Description : Data types for payment requests, statuses, and records.
--
-- This module defines core types used for payment processing,
-- including requests, statuses, and records with JSON serialization.
module Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- | Represents a payment request, specifying source and destination accounts
--   along with the amount to transfer.
data PaymentRequest = PaymentRequest
  { fromAccount :: Text   -- ^ Source account identifier
  , toAccount   :: Text   -- ^ Destination account identifier
  , amount      :: Double -- ^ Amount to be transferred
  } deriving (Show, Generic)

instance ToJSON PaymentRequest
instance FromJSON PaymentRequest

-- | Represents the status of a payment transaction.
data PaymentStatus = Success | Failed
  deriving (Show, Generic)

instance ToJSON PaymentStatus
instance FromJSON PaymentStatus

-- | A complete record of a payment transaction.
data PaymentRecord = PaymentRecord
  { paymentId  :: Int           -- ^ Unique payment identifier
  , fromAcc    :: Text          -- ^ Source account identifier
  , toAcc      :: Text          -- ^ Destination account identifier
  , amountPaid :: Double        -- ^ Amount transferred
  , status    :: PaymentStatus  -- ^ Result of the payment
  , timestamp  :: UTCTime       -- ^ Time when the payment was made
  } deriving (Show, Generic)

instance ToJSON PaymentRecord
instance FromJSON PaymentRecord
