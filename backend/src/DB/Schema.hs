{-# LANGUAGE OverloadedStrings #-}

module DB.Schema (createTables) where

import Database.PostgreSQL.Simple (Connection, execute_)

createTables :: Connection -> IO ()
createTables conn = do
    putStrLn "Ensuring database schema exists..."
    -- Merchant table
    execute_ conn
      "CREATE TABLE IF NOT EXISTS merchants (\
      \  merchant_id SERIAL PRIMARY KEY,\
      \  name TEXT NOT NULL,\
      \  email TEXT UNIQUE NOT NULL,\
      \  password_hash TEXT NOT NULL,\
      \  api_key TEXT UNIQUE,\
      \  balance DOUBLE PRECISION DEFAULT 0\
      \);"

    -- Payments table
    execute_ conn
      "CREATE TABLE IF NOT EXISTS payments (\
      \ payment_id SERIAL PRIMARY KEY,\
      \  merchant_id INTEGER REFERENCES merchants(merchant_id),\
      \  amount DOUBLE PRECISION,\
      \  status TEXT DEFAULT 'PENDING',\
      \  created_at TIMESTAMPTZ DEFAULT NOW(),\
      \  processed_at TIMESTAMPTZ\
      \);"
    putStrLn "Schema is ready."

