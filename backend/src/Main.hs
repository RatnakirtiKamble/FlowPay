{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant
import Servant.Auth.Server
    ( JWTSettings, defaultCookieSettings,
      defaultJWTSettings, generateKey, CookieSettings(..))
import Crypto.JOSE.JWK (JWK)

import Api (api, server)
import App (App(..), AppEnv(..))
import DB.Init (getDBConnection)
import DB.Schema (createTables)

nt :: AppEnv -> App a -> Handler a
nt env app = runReaderT (runApp app) env

main :: IO ()
main = do
    key <- generateKey

    putStrLn "Initializing PostgreSQL connection..."
    conn <- getDBConnection
    putStrLn "Connection successful."

    createTables conn

    let jwtSettings = defaultJWTSettings key
    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

    let env = AppEnv
          { dbConnection = conn
          , cookieCfg = cookieSettings
          , jwtCfg = jwtSettings
          }

    let context = cookieSettings :. jwtSettings :. EmptyContext

    let appWai = serveWithContext api context (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

    -- CORS policy:
    let corsPolicy = simpleCorsResourcePolicy
          { corsOrigins = Just (["http://localhost:5173"], True) 
          , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
          , corsRequestHeaders = ["Content-Type", "Authorization", "X-API-Key"]
          }

    let appWithCors = cors (const $ Just corsPolicy) appWai

    putStrLn "Starting backend on port 8080 with CORS enabled..."
    run 8080 appWithCors
