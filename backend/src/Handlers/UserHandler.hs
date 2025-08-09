-- src/Handlers/UserHandler.hs
{-# LANGUAGE OverloadedStrings #-}

module Handlers.UserHandler (userServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Database.PostgreSQL.Simple (query_) -- Corrected this line
import Servant

import App (App, AppEnv(..)) -- Corrected this line
import Models.User (User)

-- | The server logic for the /users endpoint.
userServer :: App [User]
userServer = do
  conn <- asks dbConnection
  liftIO $ query_ conn "SELECT * FROM users"