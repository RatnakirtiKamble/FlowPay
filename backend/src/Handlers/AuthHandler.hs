{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handlers.AuthHandler (registerHandler, loginHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Database.PostgreSQL.Simple (query)
import Servant
import Servant.Auth.Server (acceptLogin)
import Web.Cookie (SetCookie)

import Crypto.Random (getRandomBytes)
import Data.ByteString.Base64.URL (encodeUnpadded)
import Data.Text.Encoding (decodeUtf8)

import App (App, AppEnv(..))
import Models.Merchant
  ( RegistrationRequest(..)
  , LoginRequest(..)
  , Merchant(..)
  , PublicMerchant(..)
  , toPublicMerchant
  )

-- | Handles merchant registration.
registerHandler :: RegistrationRequest -> App PublicMerchant
registerHandler req = do
  mHashedPasswordBytes <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack . T.unpack $ reqPassword req)
  case mHashedPasswordBytes of
    Nothing -> throwError err500 { errBody = "Server error: Password hashing failed." }
    Just hashedPasswordBytes -> do
      let hashedPassword = unpack hashedPasswordBytes
      conn <- asks dbConnection
      apiKey <- liftIO generateApiKey
      [newMerchant] <- liftIO $ query conn
        "INSERT INTO merchants (name, email, password_hash, api_key, balance) VALUES (?, ?, ?, ?, 0) RETURNING merchant_id, name, email, api_key, password_hash, balance"
        (reqName req, reqEmail req, hashedPassword, apiKey)
      return $ toPublicMerchant newMerchant

-- | Handles merchant login.
loginHandler :: LoginRequest -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler req = do
  conn <- asks dbConnection
  merchants <- liftIO $ query conn "SELECT * FROM merchants WHERE email = ?" [loginEmail req]

  case merchants of
    [merchant] -> do
      let passwordValid = validatePassword (pack . T.unpack $ merchantPasswordHash merchant) (pack . T.unpack $ loginPassword req)
      if passwordValid
        then do
          cookieSettings <- asks cookieCfg
          jwtSettings <- asks jwtCfg
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings merchant
          case mApplyCookies of
            Nothing -> throwError err401
            Just applyCookies -> return $ applyCookies NoContent
        else throwError err401 { errBody = "Invalid email or password." }
    _ -> throwError err401 { errBody = "Invalid email or password." }

-- | Generate a secure random API key, base64-url encoded, unpadded.
generateApiKey :: IO Text
generateApiKey = do
  bytes <- getRandomBytes 32 
  pure $ decodeUtf8 $ encodeUnpadded bytes
