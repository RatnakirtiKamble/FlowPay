-- src/DB/Schema.hs
{-# LANGUAGE OverloadedStrings #-}

module DB.Schema (createTables) where

import Database.PostgreSQL.Simple (Connection, execute_)

createTables :: Connection -> IO ()
createTables conn = do
    putStrLn "Ensuring database schema exists..."
    -- User table with email, password hash, and balance
    execute_ conn
      "CREATE TABLE IF NOT EXISTS users (\
      \ id SERIAL PRIMARY KEY,\
      \ name TEXT NOT NULL,\
      \ email TEXT NOT NULL UNIQUE,\
      \ password_hash TEXT NOT NULL,\
      \ balance DOUBLE PRECISION NOT NULL DEFAULT 1000.0\
      \)"
    -- Payments table
    execute_ conn
      "CREATE TABLE IF NOT EXISTS payments (\
      \ id SERIAL PRIMARY KEY,\
      \ from_acc TEXT NOT NULL,\
      \ to_acc TEXT NOT NULL,\
      \ amount DOUBLE PRECISION NOT NULL,\
      \ status TEXT NOT NULL,\
      \ timestamp TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP\
      \)"
    putStrLn "Schema is ready."

-- -- | Seeds the database with some initial data.
-- seedDB :: Connection -> IO ()
-- seedDB conn = do
--     putStrLn "Seeding database with initial users..."
--     -- Using `execute` with `VALUES ?` prevents SQL injection.
--     execute conn "INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT (id) DO NOTHING" (User 1 "Alice")
--     execute conn "INSERT INTO users (id, name) VALUES (?, ?) ON CONFLICT (id) DO NOTHING" (User 2 "Bob")
--     putStrLn "Database seeded."