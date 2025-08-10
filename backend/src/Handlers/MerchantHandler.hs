{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Handlers.MerchantHandler (
    merchantServer,
    revokeApiKeyHandler,
    listPaymentsHandler,
    ApiKeyResponse(..),
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, fastBcryptHashingPolicy)
import Crypto.Random (getRandomBytes)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL (encodeUnpadded)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.PostgreSQL.Simple (query, execute, withTransaction, Only(..))
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult(..))
import App (App, dbConnection)
import Models.Merchant (Merchant(..))
import Models.Payment (Payment)

data ApiKeyResponse = ApiKeyResponse
  { apiKey :: Text
  } deriving (Generic)

instance ToJSON ApiKeyResponse

-- Generate random API key (private helper)
generateRandomApiKey :: IO Text
generateRandomApiKey = do
  bytes <- getRandomBytes 32 
  pure $ decodeUtf8 $ encodeUnpadded bytes

hashApiKey :: Text -> IO (Maybe ByteString)
hashApiKey keyText = hashPasswordUsingPolicy fastBcryptHashingPolicy (encodeUtf8 keyText)

-- Generate, hash, and update API key in DB
generateAndUpdateApiKey :: Int -> App ApiKeyResponse
generateAndUpdateApiKey merchantId = do
  conn <- asks dbConnection
  plainTextKey <- liftIO generateRandomApiKey
  maybeHashedKey <- liftIO $ hashApiKey plainTextKey

  case maybeHashedKey of
    Nothing ->
      throwError err500 { errBody = "API key hashing failed." }
      
    Just hashedKey -> do
      liftIO $ withTransaction conn $ do
        _ <- execute conn
          "UPDATE merchants SET api_key = ? WHERE merchant_id = ?" (hashedKey, merchantId)
        pure ()

      pure $ ApiKeyResponse { apiKey = plainTextKey }
      
-- The servant handler's type signature remains the same
merchantServer :: AuthResult Merchant -> App ApiKeyResponse
merchantServer (Authenticated merchant) = generateAndUpdateApiKey (merchantId merchant)
merchantServer _ = throwError err401

listPaymentsHandler :: AuthResult Merchant -> App [Payment]
listPaymentsHandler (Authenticated merchant) = do
  conn <- asks dbConnection
  let mId = merchantId merchant
  
  -- Fetch all payments for the given merchant, ordered by most recent
  liftIO $ query conn
    "SELECT payment_id, merchant_id, amount, status, created_at FROM payments WHERE merchant_id = ? ORDER BY created_at DESC"
    (Only mId)
    
listPaymentsHandler _ = throwError err401

revokeApiKeyHandler :: AuthResult Merchant -> App NoContent
revokeApiKeyHandler (Authenticated merchant) = do
  conn <- asks dbConnection
  let mId = merchantId merchant
  
  liftIO $ withTransaction conn $ do
    _ <- execute conn
      "UPDATE merchants SET api_key = NULL WHERE merchant_id = ?"
      (Only mId)
    pure ()
    
  return NoContent
revokeApiKeyHandler _ = throwError err401