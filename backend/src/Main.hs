{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- This module serves as the main entry point for the backend server.
-- It initializes the database connection, sets up the application environment,
-- configures CORS and JWT/Cookie settings, and launches the Warp server.
module Main where

-- Base Imports
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Crypto.JOSE.JWK (JWK)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Text.Encoding (decodeUtf8) -- 1. Import the decoder

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

-- |
-- The Natural Transformation (nt) function.
-- This is the crucial bridge that converts our custom 'App' monad into Servant's 'Handler' monad.
-- It "runs" the entire monad stack (ReaderT, ExceptT, LoggingT) for each request.
nt :: AppEnv -> App a -> Handler a
nt env app = do
  let loggingAction = logToFile "app.log"
  -- Correctly run the monad stack from the inside out:
  -- 1. Run the ReaderT with the environment.
  -- 2. Run the ExceptT to handle potential errors.
  -- 3. Run the LoggingT to perform the logging action.
  -- 4. Lift the final IO action into the Handler monad.
  result <- liftIO . runLoggingT (runExceptT (runReaderT (runApp app) env)) $ loggingAction
  case result of
    Left err -> throwError err
    Right a  -> return a

-- |
-- A custom logging action that formats messages with a timestamp and level
-- before appending them to a specified file.
logToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToFile path _ _ level msg = do
  timestamp <- pack . formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" <$> getCurrentTime
  -- 2. Convert the ByteString from the logger to Text before appending
  TIO.appendFile path $ timestamp <> " [" <> pack (show level) <> "] " <> decodeUtf8 (fromLogStr msg) <> "\n"

-- |
-- Defines the Cross-Origin Resource Sharing (CORS) policy for the server.
-- This policy allows requests from the frontend development server (localhost:5173)
-- and specifies which methods and headers are permitted.
corsWithCredentials :: Middleware
corsWithCredentials = cors $ const $ Just $
    simpleCorsResourcePolicy
        { corsOrigins        = Just (["http://localhost:5173"], True)
        , corsMethods        = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key", "X-XSRF-Token"]
        }

-- |
-- The main entry point of the application.
main :: IO ()
main = do
    -- Generate a key for signing JWTs
    key <- generateKey

    -- Initialize database connection and create tables
    putStrLn "Initializing PostgreSQL connection..."
    conn <- getDBConnection
    putStrLn "Connection successful."
    createTables conn

    -- Configure JWT and Cookie settings
    let jwtSettings = defaultJWTSettings key
    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

    -- Create the application environment
    let env = AppEnv
          { dbConnection = conn
          , cookieCfg = cookieSettings
          , jwtCfg = jwtSettings
          }

    -- Define the context for servant-auth
    let context = cookieSettings :. jwtSettings :. EmptyContext

    -- Create the WAI application by serving the API
    let appWai =
          serveWithContext api context
              (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

    putStrLn "Starting backend on port 8080 with CORS and credentials..."
    run 8080 $ corsWithCredentials appWai
