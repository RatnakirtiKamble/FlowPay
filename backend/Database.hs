{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Database
-- Description : Database connection utilities for the payments backend.
--
-- This module provides functions to establish a connection
-- to the PostgreSQL database using the default connection info.
module Database where

import Types
import Database.PostgreSQL.Simple
import Data.Time (getCurrentTime)

-- | Establishes a connection to the PostgreSQL database using default settings.
--
-- Connection details are hardcoded here for localhost access:
--  * Host: "localhost"
--  * Database: "payments_db"
--  * User: "postgres"
--  * Password: "yourpassword"
--
-- In production, consider moving these to environment variables or configuration files.
connectDB :: IO Connection
connectDB = connect defaultConnectInfo
  { connectHost = "localhost"
  , connectDatabase = "payments_db"
  , connectUser = "postgres"
  , connectPassword = "yourpassword"
  }
