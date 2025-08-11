{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- |
-- Module      : Main
-- Description : Main entry point for the backend server.
--
-- This module initializes the application environment, including:
--  * Database connection and schema setup
--  * JWT signing key generation and cookie configuration
--  * CORS policy allowing requests from the frontend development server
--  * Logging setup with timestamps to a file
--  * Starts the Warp server on a port specified by the PORT environment variable,
--    defaulting to 8080.
--
-- The application monad 'App' is converted to Servant's 'Handler' monad via the natural
-- transformation 'nt', which runs the monad transformer stack (ReaderT, ExceptT, LoggingT).
module Main where

-- Base Imports

-- Servant & Wai Imports

-- Logging Imports

-- Internal Project Imports
import Api (api, server)
import App (App (..), AppEnv (..))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc (..), LogLevel (..), LogSource, LogStr, fromLogStr, runLoggingT)
import Control.Monad.Reader (runReaderT)
import Crypto.JOSE.JWK (JWK)
import DB.Init (getDBConnection)
import DB.Schema (createTables)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server
import qualified Servant.Auth.Server as SAS
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Web.Cookie(sameSiteNone)

-- | Natural transformation from the application monad 'App' to Servant's 'Handler'.
--
-- Runs the monad transformer stack: ReaderT for environment,
-- ExceptT for error handling, LoggingT for structured logging,
-- lifting the final IO action into the Servant 'Handler' monad.
--
-- Logs all messages to a file ("app.log") with timestamps and log levels.
nt :: AppEnv -> App a -> Handler a
nt env app = do
  let loggingAction = logToFile "app.log"
  result <- liftIO . runLoggingT (runExceptT (runReaderT (runApp app) env)) $ loggingAction
  case result of
    Left err -> throwError err
    Right a -> return a

-- | Logging action that writes log messages to a specified file,
-- prefixed with a timestamp and the log level.
logToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToFile path _ _ level msg = do
  timestamp <- pack . formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" <$> getCurrentTime
  TIO.appendFile path $ timestamp <> " [" <> pack (show level) <> "] " <> decodeUtf8 (fromLogStr msg) <> "\n"

-- | CORS middleware allowing requests from the frontend dev server at localhost:5173,
-- enabling credentials and common HTTP methods and headers needed for authentication.
corsWithCredentials :: Middleware
corsWithCredentials = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins = Just (["https://flowpayratna.netlify.app", "http://localhost:5173"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key", "X-XSRF-Token", "X-Xsrf-Token", "X-XSRF-TOKEN"]
      }

-- | Main application entry point.
--
-- Performs the following steps:
-- 1. Reads the PORT environment variable, defaulting to 8080.
-- 2. Generates a cryptographic key for JWT signing.
-- 3. Initializes the PostgreSQL connection and ensures database schema is created.
-- 4. Sets up JWT and Cookie authentication settings.
-- 5. Constructs the application environment.
-- 6. Builds the WAI application serving the Servant API with authentication context.
-- 7. Runs the Warp server with CORS enabled on the specified port.
main :: IO ()
main = do
  -- 1. Read port from environment variable, default to 8080 for local dev
  portStr <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portStr >>= readMaybe)

  -- Generate key for JWT signing
  key <- generateKey

  -- Setup database connection and create tables
  putStrLn "Initializing PostgreSQL connection..."
  conn <- getDBConnection
  putStrLn "Connection successful."
  createTables conn

  -- Setup JWT and cookie settings
  let jwtSettings = defaultJWTSettings key

  let cookieSettings =
        SAS.defaultCookieSettings
          { SAS.cookieIsSecure = SAS.Secure,
            SAS.cookieSameSite = SAS.AnySite
          }

  -- Create app environment
  let env =
        AppEnv
          { dbConnection = conn,
            cookieCfg = cookieSettings,
            jwtCfg = jwtSettings
          }

  -- Compose Servant authentication context
  let context = cookieSettings :. jwtSettings :. EmptyContext

  -- Build WAI application with auth context and natural transformation
  let appWai =
        serveWithContext
          api
          context
          (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

  putStrLn $ "Starting backend on port " ++ show port ++ " with CORS and credentials..."
  run port $ corsWithCredentials appWai