-- src/Handlers/PaymentHandler.hs
{-# LANGUAGE OverloadedStrings #-}

module Handlers.PaymentHandler (paymentServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Servant

import App (App, AppEnv(..))
import Models.Payment (PaymentRequest(..), PaymentResponse, Payment(..), PaymentStatus(..))
import Database.PostgreSQL.Simple (query)

paymentServer :: PaymentRequest -> App PaymentResponse
paymentServer req = do
  -- Basic validation
  if amount req <= 0
    then throwError $ err400 { errBody = "Payment amount must be positive." }
    else do
      conn <- asks dbConnection
      liftIO $ putStrLn $ "Processing payment request: " ++ show req

      -- Use PostgreSQL's `RETURNING` clause to get the new row in one query.
      results <- liftIO $ query conn
        "INSERT INTO payments (from_acc, to_acc, amount, status) VALUES (?, ?, ?, ?) \
        \ RETURNING id, from_acc, to_acc, amount, status, timestamp"
        (fromAccount req, toAccount req, amount req, Success)

      -- Safely handle the result from the database query
      case results of
        [newPayment] -> do
          liftIO $ putStrLn $ "Successfully processed and stored: " ++ show newPayment
          return newPayment
        _ -> do
          -- This case should not happen with a RETURNING clause on a single INSERT,
          -- but it's robust to handle it. It indicates a server-side issue.
          liftIO $ putStrLn "Error: Database did not return a single row after insert."
          throwError err500 { errBody = "Could not confirm payment creation." }