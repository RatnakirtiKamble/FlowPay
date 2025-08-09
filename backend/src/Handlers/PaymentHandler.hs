{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.PaymentHandler (paymentServer, PaymentRequest(..), PaymentResponse(..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Time.Clock (getCurrentTime)
import Database.PostgreSQL.Simple (query, execute, Only(..))
import Servant
import Data.Text (Text)
import qualified Data.Text as T

import App (App, dbConnection)
import Models.Merchant (Merchant(..))
import Models.Payment (PaymentRequest(..), PaymentResponse(..))

-- | Handle payment with API key passed via header
paymentHandler :: Maybe Text -> PaymentRequest -> App PaymentResponse
paymentHandler Nothing _ = throwError err401 { errBody = "Missing API key in header." }
paymentHandler (Just key) paymentReq = do
  let amt = paymentAmount paymentReq
  if amt <= 0
    then throwError err400 { errBody = "Payment amount must be positive." }
    else do
          conn <- asks dbConnection
          currentTime <- liftIO getCurrentTime

          -- Query merchant by API key
          merchants <- liftIO $ query conn
            "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE api_key = ?"
            (Only key)

          case merchants of
            [merchant] -> do
              -- Insert payment as "Pending"
              _ <- liftIO $ execute conn
                "INSERT INTO payments (merchant_id, amount, status, created_at) VALUES (?, ?, ?, ?)"
                (merchantId merchant, amt, ("Pending" :: Text), currentTime)

              -- Update merchant balance immediately (demo)
              _ <- liftIO $ execute conn
                "UPDATE merchants SET balance = balance + ? WHERE merchant_id = ?"
                (amt, merchantId merchant)

              -- Update payment status to "Success"
              _ <- liftIO $ execute conn
                "UPDATE payments SET status = ? WHERE merchant_id = ? AND amount = ? AND created_at = ?"
                ("Success" :: Text, merchantId merchant, amt, currentTime)

              -- Fetch updated balance
              results <- liftIO $ query conn
                "SELECT balance FROM merchants WHERE merchant_id = ?"
                (Only $ merchantId merchant)

              case results of
                [Only newBalance] -> return $ PaymentResponse
                  { paymentStatus = "Success"
                  , newBalance = newBalance
                  }
                _ -> throwError err500 { errBody = "Failed to fetch updated balance." }

            _ -> throwError err401 { errBody = "Invalid API key." }

-- | Servant API type: Accept API key header plus payment request body
type PaymentAPI =
  "payments" :> Header "X-API-KEY" Text :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse

paymentServer :: ServerT PaymentAPI App
paymentServer = paymentHandler
