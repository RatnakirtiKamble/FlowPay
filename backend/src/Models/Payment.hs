{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Models.Payment
Description : Data models for payments, including database entities, 
              API requests, and responses.

This module defines:
  * Full payment records (as stored in DB)
  * Request/response types for payment processing
  * JSON and DB row instances
-}
module Models.Payment where

-- ========== Imports ==========
import Data.Aeson
  ( ToJSON(..), FromJSON(..), object, (.=) )
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)

-- ========== Payment Entity (Full DB Record) ==========

-- | Represents a single payment record as stored in the database.
-- Used for sending transaction history to the dashboard.
data Payment = Payment
  { paymentId  :: Int       -- ^ Unique payment ID
  , merchantId :: Int       -- ^ Associated merchant ID
  , amount     :: Double    -- ^ Payment amount
  , status     :: Text      -- ^ Payment status (e.g., "completed", "failed")
  , createdAt  :: UTCTime   -- ^ Timestamp when payment was created
  } deriving (Generic, Show)

-- | Serialize payment to JSON for API responses.
instance ToJSON Payment

-- | Map database rows to 'Payment'.
instance FromRow Payment

-- ========== Payment Request & Response DTOs ==========

-- | API request payload to initiate a payment.
data PaymentRequest = PaymentRequest
  { paymentAmount :: Double -- ^ Amount to be paid
  } deriving (Show, Generic)

instance FromJSON PaymentRequest
instance ToJSON PaymentRequest

-- | API response payload after processing a payment.
data PaymentResponse = PaymentResponse
  { transactionId :: Int    -- ^ Unique transaction ID
  , paymentStatus :: Text   -- ^ Status of the payment
  , amount        :: Double -- ^ Amount processed
  , newBalance    :: Double -- ^ Updated merchant balance
  } deriving (Show, Generic)

-- | Custom JSON encoding to avoid field name collisions and ensure clear API output.
instance ToJSON PaymentResponse where
  toJSON (PaymentResponse { transactionId = tid
                          , paymentStatus = ps
                          , amount = amt
                          , newBalance = nb
                          }) =
    object
      [ "transaction_id" .= tid
      , "payment_status" .= ps
      , "amount"         .= amt
      , "new_balance"    .= nb
      ]

instance FromJSON PaymentResponse
