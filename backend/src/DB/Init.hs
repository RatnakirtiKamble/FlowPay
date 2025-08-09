-- src/DB/Init.hs
{-# LANGUAGE OverloadedStrings #-}

module DB.Init (getDBConnection) where

import Database.PostgreSQL.Simple (Connection, connect, defaultConnectInfo, connectHost, connectUser, connectPassword, connectDatabase)

-- | Creates and returns a new connection to the PostgreSQL database.
-- | This function should be called once when the application starts.
getDBConnection :: IO Connection
getDBConnection = connect defaultConnectInfo
    { connectHost     = "localhost"
    , connectUser     = "ratna"
    , connectPassword = "Ratna@1234"
    , connectDatabase = "payments_db"
    }