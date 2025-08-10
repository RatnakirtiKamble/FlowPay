{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-} 

module Models.Payment where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow)

-- This new data type represents a single payment record from your database.
-- It will be used to send the transaction history to your dashboard.
data Payment = Payment
  { paymentId   :: Int
  , merchantId  :: Int
  , amount      :: Double -- Now allowed due to the extension
  , status      :: Text
  , createdAt   :: UTCTime
  } deriving (Generic, Show)

-- This instance allows the Payment type to be converted to JSON for the API response.
instance ToJSON Payment
-- This instance allows the Payment type to be read from a database row.
instance FromRow Payment


-- Your existing data types below are unchanged.

data PaymentRequest = PaymentRequest
  { paymentAmount :: Double
  } deriving (Show, Generic)

instance FromJSON PaymentRequest
instance ToJSON PaymentRequest

data PaymentResponse = PaymentResponse
  { transactionId :: Int
  , paymentStatus :: Text
  , amount        :: Double -- Now allowed due to the extension
  , newBalance    :: Double
  } deriving (Show, Generic)

-- THIS IS THE FIX: Use pattern matching to explicitly bring the fields into scope,
-- which resolves the ambiguity for the 'amount' field.
instance ToJSON PaymentResponse where
    toJSON (PaymentResponse { transactionId = tid, paymentStatus = ps, amount = amt, newBalance = nb }) =
        object
            [ "transaction_id" .= tid
            , "payment_status" .= ps
            , "amount"         .= amt
            , "new_balance"    .= nb
            ]

instance FromJSON PaymentResponse
