-- src/Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Monad.Reader (runReaderT)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
    ( JWTSettings, defaultCookieSettings,
      defaultJWTSettings, generateKey, FromJWT(decodeJWT), ToJWT,
      CookieSettings(..)) -- Corrected this import
import Crypto.JOSE.JWK (JWK)

import Api (api, server)
import App (App(..), AppEnv(..))
import DB.Init (getDBConnection)
import DB.Schema (createTables)

-- | This function converts our `App` monad into the `Handler` monad
-- | that Servant understands by providing the application environment.
nt :: AppEnv -> App a -> Handler a
nt env app = runReaderT (runApp app) env

main :: IO ()
main = do
    -- Generate a key for signing JWTs. In a real app, you would load
    -- this from a file or environment variable.
    key <- generateKey

    -- Setup the database connection.
    putStrLn "Initializing PostgreSQL connection..."
    conn <- getDBConnection
    putStrLn "Connection successful."

    -- Setup the database schema.
    createTables conn

    -- Define cookie and JWT settings.
    let jwtSettings = defaultJWTSettings key
    -- The FIX is here: we override the 'secure' setting for development.
    let cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

    -- Create the application environment.
    let env = AppEnv
          { dbConnection = conn
          , cookieCfg = cookieSettings
          , jwtCfg = jwtSettings
          }
    
    -- `Context` is used by servant-auth to pass settings to the middleware.
    let context = cookieSettings :. jwtSettings :. EmptyContext

    -- Hoist the server to run in our App monad, providing the context.
    let app = serveWithContext api context (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (nt env) server)

    putStrLn "Starting backend on port 8080..."
    run 8080 app
