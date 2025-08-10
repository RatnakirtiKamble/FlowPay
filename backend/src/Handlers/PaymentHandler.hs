{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Handlers.PaymentHandler
-- Description : Handler for processing payment requests with API key authentication.
--
-- This module provides the payment API handler to process payment requests
-- from merchants. It validates the API key against stored merchant credentials,
-- checks for valid payment amounts, performs the payment transaction by inserting
-- a payment record and updating the merchant's balance atomically, and returns
-- the payment response including the new balance and transaction ID.
--
-- The main exported function is 'paymentServer', which implements the Servant server
-- endpoint expecting an "X-API-Key" header and a JSON payment request.
--
-- The API key is validated using bcrypt hashed keys stored in the merchants table.
--
-- Error handling includes:
--  * 401 Unauthorized if API key is missing or invalid
--  * 400 Bad Request if payment amount is zero or negative
--
module Handlers.PaymentHandler (paymentServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Logger (logInfoN, logWarnN)
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (query, execute, withTransaction, Only(..))
import Servant
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Crypto.BCrypt (validatePassword) 
import Data.Maybe (listToMaybe) 

import App (App, dbConnection)
import Models.Merchant (Merchant(..))
import Models.Payment (PaymentRequest(..), PaymentResponse(..))

-- | Validates the provided API key against a list of merchants.
-- 
-- The API key is checked by comparing it (plaintext) against the bcrypt hashed keys
-- stored in the merchants. If a matching merchant is found, returns 'Just Merchant',
-- otherwise 'Nothing'.
--
-- Parameters:
--   * providedKey - The API key sent by the client
--   * merchants - The list of merchants loaded from the database
--
-- Returns:
--   * 'Just Merchant' if API key matches any merchant's stored key
--   * 'Nothing' otherwise
validateApiKey :: Text -> [Merchant] -> Maybe Merchant
validateApiKey providedKey merchants =
  listToMaybe $ filter (checkKey providedKey) merchants
  where
    checkKey key merchant =
      case merchantApiKey merchant of
        Nothing -> False
        Just hashedKey -> validatePassword (encodeUtf8 hashedKey) (encodeUtf8 key)

-- | Handles a payment request.
--
-- This function expects an optional API key (from request headers) and a 'PaymentRequest'.
-- It performs the following:
--  1. Checks if API key is provided; if missing, returns 401 Unauthorized.
--  2. Validates the API key against merchants in the database.
--  3. Checks if the payment amount is positive; if not, returns 400 Bad Request.
--  4. On successful validation, inserts a payment record with status "Success",
--     updates the merchant's balance atomically inside a transaction, and returns
--     a 'PaymentResponse' with transaction details.
--  5. Logs relevant information or warnings during the process.
--
-- Parameters:
--   * Maybe Text - Optional API key from "X-API-Key" header
--   * PaymentRequest - The payment details sent by the client
--
-- Returns:
--   * 'PaymentResponse' on success
--   * Throws Servant errors (401 or 400) for failure cases
paymentHandler :: Maybe Text -> PaymentRequest -> App PaymentResponse
paymentHandler Nothing _ = do
    logWarnN "Payment failed: Missing API key in header."
    throwError err401 { errBody = "Missing API key in header." }
paymentHandler (Just key) paymentReq = do
  logInfoN $ "Received payment request with API key: " <> key
  let amt = paymentAmount paymentReq
  if amt <= 0
    then do
      logWarnN $ "Payment failed: Amount must be positive. Received: " <> T.pack (show amt)
      throwError err400 { errBody = "Payment amount must be positive." }
    else do
      conn <- asks dbConnection
      currentTime <- liftIO getCurrentTime
      
      allMerchants <- liftIO $ query conn "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants" ()

      case validateApiKey key allMerchants of
        Just merchant -> do
          logInfoN $ "API key validated for merchant ID: " <> T.pack (show $ merchantId merchant)
          (newBalance, paymentId) <- liftIO $ withTransaction conn $ do
            [Only pId] <- query conn
              "INSERT INTO payments (merchant_id, amount, status, created_at) VALUES (?, ?, ?, ?) RETURNING payment_id"
              (merchantId merchant, amt, ("Success" :: Text), currentTime)
            _ <- execute conn
              "UPDATE merchants SET balance = balance + ? WHERE merchant_id = ?"
              (amt, merchantId merchant)
            [Only finalBalance] <- query conn
              "SELECT balance FROM merchants WHERE merchant_id = ?"
              (Only $ merchantId merchant)
            return (finalBalance, pId)

          logInfoN $ "Payment successful. Txn ID: " <> T.pack (show paymentId) <> ", New Balance: " <> T.pack (show newBalance)
          return $ PaymentResponse
            { transactionId = paymentId
            , paymentStatus = "Success"
            , amount        = amt
            , newBalance    = newBalance
            }
        Nothing -> do
          logWarnN "Payment failed: Invalid API key provided."
          throwError err401 { errBody = "Invalid API key." }

-- | Servant server implementation for the payments endpoint.
--
-- This endpoint expects:
--  * Header "X-API-Key" containing the API key as 'Text'
--  * JSON request body conforming to 'PaymentRequest'
--
-- Returns a JSON 'PaymentResponse'.
paymentServer :: ServerT (Header "X-API-Key" Text :> "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse) App
paymentServer = paymentHandler
