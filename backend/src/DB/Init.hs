{-# LANGUAGE OverloadedStrings #-}

module DB.Init (getDBConnection) where

import Database.PostgreSQL.Simple (Connection, connect, defaultConnectInfo, connectHost, connectUser, connectPassword, connectDatabase)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

-- | Creates and returns a new connection to the PostgreSQL database
getDBConnection :: IO Connection
getDBConnection = do 
    host <- fromMaybe "localhost" <$> lookupEnv "PGHOST"
    db   <- fromMaybe "payments_db" <$> lookupEnv "PGDATABASE"
    user <- fromMaybe "postgres" <$> lookupEnv "PGUSER"
    pass <- fromMaybe "yourpassword" <$> lookupEnv "PGPASSWORD"
    connect defaultConnectInfo
        { connectHost = host
        , connectDatabase = db
        , connectUser = user
        , connectPassword = pass
        }
