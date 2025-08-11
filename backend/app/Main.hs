{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

-- Base Imports
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

-- Servant & Wai Imports
import Servant
import Servant.Auth.Server -- This now provides everything we need

-- Internal Project Imports
import Api (api, server)
import App (App (..), AppEnv (..))
import DB.Init (getDBConnection)
import DB.Schema (createTables)

-- | Natural transformation (remains the same)
nt :: AppEnv -> App a -> Handler a
nt env app = do
  let loggingAction = logToFile "app.log"
  result <- liftIO . runLoggingT (runExceptT (runReaderT (runApp app) env)) $ loggingAction
  case result of
    Left err -> throwError err
    Right a -> return a

-- | Logging action (remains the same)
logToFile :: FilePath -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logToFile path _ _ level msg = do
  timestamp <- pack . formatTime defaultTimeLocale "[%Y-%m-%d %H:%M:%S]" <$> getCurrentTime
  TIO.appendFile path $ timestamp <> " [" <> pack (show level) <> "] " <> decodeUtf8 (fromLogStr msg) <> "\n"

-- | CORS middleware (remains the same)
corsWithCredentials :: Middleware
corsWithCredentials = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
      { corsOrigins = Just (["https://flowpayratna.netlify.app", "http://localhost:5173"], True)
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key", "X-XSRF-Token", "X-Xsrf-Token", "X-XSRF-TOKEN"]
      }

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = fromMaybe 8080 (portStr >>= readMaybe)

  key <- generateKey

  putStrLn "Initializing PostgreSQL connection..."
  conn <- getDBConnection
  putStrLn "Connection successful."
  createTables conn

  -- Setup JWT settings (this is all we need now)
  let jwtSettings = defaultJWTSettings key

  let cookieSettings = defaultCookieSettings

  -- Create app environment without cookie config
  let env =
        AppEnv
          { dbConnection = conn,
            cookieCfg = cookieSettings,
            jwtCfg = jwtSettings
          }

  -- Compose Servant authentication context for JWT only
  let context = cookieSettings :. jwtSettings :. EmptyContext

  -- Build WAI application with the simplified auth context
  let appWai =
        serveWithContext
          api
          context
          (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)


  putStrLn $ "Starting backend on port " ++ show port ++ " with CORS and credentials..."
  run port $ corsWithCredentials appWai