{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Main
-- Description : Main entry point for the backend server.
--
-- This module initializes the application environment, including:
--  * Database connection and schema setup
--  * JWT signing key generation and cookie configuration
--  * CORS policy allowing requests from the frontend development server
--  * Logging setup with timestamps to a file
--  * Starts the Warp server on port 8080 serving the Servant API with authentication
--
-- The application monad 'App' is converted to Servant's 'Handler' monad via the natural
-- transformation 'nt', which runs the monad transformer stack (ReaderT, ExceptT, LoggingT).
module Main where

-- Base Imports
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Crypto.JOSE.JWK (JWK)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text.Encoding (decodeUtf8)

-- Servant & Wai Imports
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server

-- Logging Imports
import Control.Monad.Logger (runLoggingT, fromLogStr, Loc(..), LogLevel(..), LogSource, LogStr)

-- Internal Project Imports
import Api (api, server)
import App (App(..), AppEnv(..))
import DB.Init (getDBConnection)
import DB.Schema (createTables)

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
    Right a  -> return a

-- | Logging action that writes log messages to a specified file,
-- prefixed with a timestamp and the log level.
logToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToFile path _ _ level msg = do
  timestamp <- pack . formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" <$> getCurrentTime
  TIO.appendFile path $ timestamp <> " [" <> pack (show level) <> "] " <> decodeUtf8 (fromLogStr msg) <> "\n"

-- | CORS middleware allowing requests from the frontend dev server at localhost:5173,
-- enabling credentials and common HTTP methods and headers needed for authentication.
corsWithCredentials :: Middleware
corsWithCredentials = cors $ const $ Just $
    simpleCorsResourcePolicy
        { corsOrigins        = Just (["https://flowpayratna.netlify.app", "http://localhost:5173"], True)
        , corsMethods        = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key", "X-XSRF-Token"]
        }

-- | Main application entry point.
--
-- Performs the following steps:
-- 1. Generates a cryptographic key for JWT signing.
-- 2. Initializes the PostgreSQL connection and ensures database schema is created.
-- 3. Sets up JWT and Cookie authentication settings.
-- 4. Constructs the application environment.
-- 5. Builds the WAI application serving the Servant API with authentication context.
-- 6. Runs the Warp server with CORS enabled on port 8080.
main :: IO ()
main = do
    -- Generate key for JWT signing
    key <- generateKey

    -- Setup database connection and create tables
    putStrLn "Initializing PostgreSQL connection..."
    conn <- getDBConnection
    putStrLn "Connection successful."
    createTables conn

    -- Setup JWT and cookie settings
    let jwtSettings = defaultJWTSettings key
    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

    -- Create app environment
    let env = AppEnv
          { dbConnection = conn
          , cookieCfg = cookieSettings
          , jwtCfg = jwtSettings
          }

    -- Compose Servant authentication context
    let context = cookieSettings :. jwtSettings :. EmptyContext

    -- Build WAI application with auth context and natural transformation
    let appWai =
          serveWithContext api context
              (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

    putStrLn "Starting backend on port 8080 with CORS and credentials..."
    run 8080 $ corsWithCredentials appWai
