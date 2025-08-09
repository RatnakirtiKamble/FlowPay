{-# LANGUAGE OverloadedStrings #-}

module Database where

import Types
import Database.PostgreSQL.Simple
import Data.Time (getCurrentTime)

connectDB :: IO Connection
connectDB = connect defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "payments_db"
  , connectUser = "postgres"
  , connectPassword = "yourpassword"
  }

insertPayment :: Connection -> PaymentRequest -> PaymentStatus -> IO ()
insertPayment conn (PaymentRequest from to amt) status = do
    now <- getCurrentTime
    _ <- execute conn
      "INSERT INTO payments (from_acc, to_acc, amount, status, timestamp) VALUES (?,?,?,?,?)"
      (from, to, amt, show status, now)
    return ()
