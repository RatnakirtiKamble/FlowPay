{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : DB.Schema
-- Description : Defines and initializes the database schema.
--
-- This module provides a function to create the necessary tables for the application
-- in a PostgreSQL database if they do not already exist.
-- 
-- It creates two tables:
--   * merchants: stores merchant information such as id, name, email, password hash, API key, and balance.
--   * payments: stores payment records linked to merchants, with amount, status, and timestamps.
--
-- The 'createTables' function should be called during application startup to ensure the schema is ready.
module DB.Schema (createTables) where

import Database.PostgreSQL.Simple (Connection, execute_)

-- | Creates the required tables for the application if they do not exist.
--
-- Specifically, it creates:
--
-- * 'merchants' table with columns:
--    - merchant_id: serial primary key
--    - name: text, not null
--    - email: unique text, not null
--    - password_hash: text, not null
--    - api_key: unique text, nullable
--    - balance: double precision, defaults to 0
--
-- * 'payments' table with columns:
--    - payment_id: serial primary key
--    - merchant_id: foreign key referencing merchants(merchant_id)
--    - amount: double precision
--    - status: text, defaults to 'PENDING'
--    - created_at: timestamp with time zone, defaults to current time
--    - processed_at: timestamp with time zone, nullable
--
-- The function prints status messages to standard output.
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
