-- src/App.hs
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App (App(..), AppEnv(..), dbConnection) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import Database.PostgreSQL.Simple (Connection)
import Servant (Handler, ServerError)
import Servant.Auth.Server (CookieSettings, JWTSettings)
import Control.Monad.Logger (LoggingT, MonadLogger)
import Control.Monad.Except (ExceptT, MonadError)
-- | The environment for the application, containing the database connection and settings for cookies and JWT.
data AppEnv = AppEnv
  { dbConnection :: Connection
  , cookieCfg    :: CookieSettings
  , jwtCfg       :: JWTSettings
  }

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
