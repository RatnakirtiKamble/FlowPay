{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

{-|
Module      : Main
Description : Entry point for the FlowPay backend application.

This module:
  * Initializes environment variables, database, and authentication settings
  * Configures CORS policy with credentials
  * Sets up JWT authentication context
  * Runs the Warp server with logging and middleware

The application follows a functional programming architecture, using:
  - Haskell Servant for API definition
  - PostgreSQL for persistence
  - JWT for stateless authentication
  - Custom logging to a file
-}
module Main where

-- ========== Base Imports ==========
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (Loc (..), LogLevel (..), LogSource, LogStr, fromLogStr, runLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- ========== Servant & Wai ==========
import Servant
import Servant.Auth.Server

-- ========== Internal Project Imports ==========
import Api (api, server)
import App (App (..), AppEnv (..))
import DB.Init (getDBConnection)
import DB.Schema (createTables)

-- | Natural transformation: Transforms 'App' monad into Servant 'Handler'
-- Runs with logging and error handling.
nt :: AppEnv -> App a -> Handler a
nt env app = do
  let loggingAction = logToFile "app.log"
  result <- liftIO . runLoggingT (runExceptT (runReaderT (runApp app) env)) $ loggingAction
  case result of
    Left err -> throwError err
    Right a  -> return a

-- | Logging function: Appends timestamped log messages to a file.
logToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToFile path _ _ level msg = do
  timestamp <- pack . formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" <$> getCurrentTime
  TIO.appendFile path $
    timestamp <> " [" <> pack (show level) <> "] " <> decodeUtf8 (fromLogStr msg) <> "\n"

-- | CORS middleware configuration with credentials support.
-- Allows specified origins and required headers for API access.
corsWithCredentials :: Middleware
corsWithCredentials = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins        = Just (["https://flowpayratna.netlify.app"], True)
      , corsMethods        = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key", "X-XSRF-Token", "X-Xsrf-Token", "X-XSRF-TOKEN"]
      }

-- | Main entry point:
--  1. Loads configuration (port, JWT keys, DB connection)
--  2. Creates tables if not present
--  3. Configures authentication context
--  4. Starts the Warp server with CORS and logging
main :: IO ()
main = do
  -- Read port from environment (defaults to 8080)
  portStr <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portStr >>= readMaybe)

  -- Generate signing key for JWT authentication
  key <- generateKey

  -- Initialize database connection
  putStrLn "Initializing PostgreSQL connection..."
  conn <- getDBConnection
  putStrLn "Connection successful."
  createTables conn

  -- Setup JWT & cookie settings
  let jwtSettings    = defaultJWTSettings key
      cookieSettings = defaultCookieSettings

  -- Construct application environment
  let env = AppEnv
        { dbConnection = conn
        , cookieCfg    = cookieSettings
        , jwtCfg       = jwtSettings
        }

  -- Servant authentication context (JWT + Cookies)
  let context = cookieSettings :. jwtSettings :. EmptyContext

  -- Build WAI application
  let appWai =
        serveWithContext
          api
          context
          (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

  -- Start server with CORS enabled
  putStrLn $ "Starting backend on port " ++ show port ++ " with CORS and credentials..."
  run port $ corsWithCredentials appWai
