{-# LANGUAGE OverloadedStrings #-}

module Handlers.MerchantHandler (merchantServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL (encodeUnpadded)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple (execute)
import Servant
import Servant.Auth.Server (AuthResult(..))
import App (App, dbConnection)
import Models.Merchant (Merchant(..))

-- Generate random API key (private helper)
generateRandomApiKey :: IO Text
generateRandomApiKey = do
  bytes <- getRandomBytes 32 
  pure $ decodeUtf8 $ encodeUnpadded bytes

-- Generate and update API key in DB
generateAndUpdateApiKey :: Int -> App Text
generateAndUpdateApiKey merchantId = do
  conn <- asks dbConnection
  newKey <- liftIO generateRandomApiKey
  _ <- liftIO $ execute conn
    "UPDATE merchants SET api_key = ? WHERE merchant_id = ?" (newKey, merchantId)
  pure newKey

-- The servant handler for the protected route
merchantServer :: AuthResult Merchant -> App Text
merchantServer (Authenticated merchant) = generateAndUpdateApiKey (merchantId merchant)
merchantServer _ = throwError err401
