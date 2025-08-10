-- src/App.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : App
-- Description : Defines the application environment and the App monad transformer stack.
--
-- This module defines:
--  * 'AppEnv' — the environment containing the database connection and authentication settings.
--  * 'App' — a newtype wrapper around a monad transformer stack combining:
--      - ReaderT for dependency injection of 'AppEnv'
--      - ExceptT for Servant error handling
--      - LoggingT for structured logging
--      - IO as the base monad
--
-- The 'App' monad implements standard typeclasses for convenient use in handlers,
-- such as MonadIO, MonadReader, MonadError, MonadLogger, etc.
module App (App(..), AppEnv(..), dbConnection) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import Database.PostgreSQL.Simple (Connection)
import Servant (Handler, ServerError)
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Except (ExceptT, MonadError)

-- | The application environment containing shared resources and configuration.
data AppEnv = AppEnv
  { dbConnection :: Connection        -- ^ PostgreSQL database connection
  , cookieCfg    :: CookieSettings    -- ^ Configuration for authentication cookies
  , jwtCfg       :: JWTSettings       -- ^ Configuration for JWT authentication
  }

-- | The main application monad stack.
--
-- Wraps a 'ReaderT' over 'AppEnv' to provide access to shared environment.
-- Uses 'ExceptT' to handle Servant's 'ServerError's.
-- Uses 'LoggingT' for structured logging.
-- Runs in 'IO' at the base.
newtype App a = App { runApp :: ReaderT AppEnv (ExceptT ServerError (LoggingT IO)) a }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadReader AppEnv,
    MonadIO,
    MonadError ServerError,
    MonadFail,
    MonadLogger
  )
