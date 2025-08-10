{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

-- |
-- Module      : Handlers.MerchantHandler
-- Description : Handlers related to merchant API key management and payment listing.
--
-- This module provides Servant handlers for:
--  * Generating and updating a new API key for an authenticated merchant.
--  * Revoking (clearing) the current API key.
--  * Listing all payments made by the authenticated merchant.
--
-- API keys are securely generated as random 32-byte values, Base64 URL encoded,
-- then hashed using bcrypt before being stored in the database.
-- The plaintext API key is returned only once upon generation.
--
-- The handlers enforce authentication via 'Servant.Auth.Server.AuthResult' of 'Merchant'.
-- Unauthorized access results in HTTP 401 responses.
--
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

-- | Response type containing the plaintext API key after generation.
newtype ApiKeyResponse = ApiKeyResponse
  { apiKey :: Text 
  } deriving (Generic)

instance ToJSON ApiKeyResponse

-- | Generates a random 32-byte API key, Base64 URL encoded.
generateRandomApiKey :: IO Text
generateRandomApiKey = do
  bytes <- getRandomBytes 32 
  pure $ decodeUtf8 $ encodeUnpadded bytes

-- | Hashes the given API key text using bcrypt.
--
-- Returns 'Nothing' if hashing fails.
hashApiKey :: Text -> IO (Maybe ByteString)
hashApiKey keyText = hashPasswordUsingPolicy fastBcryptHashingPolicy (encodeUtf8 keyText)

-- | Generates a new API key, hashes it, updates it in the database for the given merchant,
-- and returns the plaintext API key.
--
-- Throws HTTP 500 error if hashing fails.
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
      
-- | Servant handler to generate and return a new API key for the authenticated merchant.
--
-- Returns HTTP 401 Unauthorized if authentication fails.
merchantServer :: AuthResult Merchant -> App ApiKeyResponse
merchantServer (Authenticated merchant) = generateAndUpdateApiKey (merchantId merchant)
merchantServer _ = throwError err401

-- | Handler to list all payments for the authenticated merchant.
--
-- Returns a list of 'Payment' records ordered by creation date descending.
-- Returns HTTP 401 Unauthorized if authentication fails.
listPaymentsHandler :: AuthResult Merchant -> App [Payment]
listPaymentsHandler (Authenticated merchant) = do
  conn <- asks dbConnection
  let mId = merchantId merchant
  
  liftIO $ query conn
    "SELECT payment_id, merchant_id, amount, status, created_at FROM payments WHERE merchant_id = ? ORDER BY created_at DESC"
    (Only mId)
    
listPaymentsHandler _ = throwError err401

-- | Handler to revoke (delete) the current API key for the authenticated merchant.
--
-- Sets the 'api_key' field to NULL in the database.
-- Returns HTTP 204 No Content on success.
-- Returns HTTP 401 Unauthorized if authentication fails.
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
