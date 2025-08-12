{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : Handlers.AuthHandler
Description : Authentication handlers for merchant registration, login, and logout.

This module implements:
  * Merchant registration with bcrypt password hashing
  * Login with JWT generation and XSRF token creation
  * Logout with simple authentication check

Security features:
  - Passwords are hashed using bcrypt before being stored
  - JWT-based stateless authentication
  - XSRF token generated and returned on login
  - Explicit expiry for JWT tokens (24 hours)
-}
module Handlers.AuthHandler
  ( registerHandler
  , loginHandler
  , logoutHandler
  ) where

-- ========== Imports ==========
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.PostgreSQL.Simple (query)
import Servant
import Servant.Auth.Server
import Data.Time.Clock (secondsToNominalDiffTime, getCurrentTime, addUTCTime)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64
import System.Random (randoms, getStdGen)

import App (App, AppEnv(..))
import Models.Merchant

-- ========== Handlers ==========

-- | Registers a new merchant by hashing their password and inserting them into the database.
registerHandler :: RegistrationRequest -> App PublicMerchant
registerHandler req = do
  mHashedPasswordBytes <- liftIO $
    hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ reqPassword req)
  case mHashedPasswordBytes of
    Nothing -> throwError err500 { errBody = "Server error: Password hashing failed." }
    Just hashedPasswordBytes -> do
      let hashedPassword = decodeUtf8 hashedPasswordBytes
      conn <- asks dbConnection
      [newMerchant] <- liftIO $
        query conn
          "INSERT INTO merchants (name, email, password_hash, balance) \
          \VALUES (?, ?, ?, 0) \
          \RETURNING merchant_id, name, email, password_hash, api_key, balance"
          (reqName req, reqEmail req, hashedPassword)
      return $ toPublicMerchant newMerchant

-- | Generates a 32-character random XSRF token encoded in Base64.
generateXsrfToken :: IO Text
generateXsrfToken =
  decodeUtf8 . B64.encode . B8.pack . take 32 . randoms <$> getStdGen

-- | Authenticates a merchant, issues a JWT and XSRF token if valid credentials are provided.
-- JWT expiry is set to 24 hours.
loginHandler :: LoginRequest -> App LoginResponse
loginHandler req = do
  conn <- asks dbConnection
  let sqlQuery =
        "SELECT merchant_id, name, email, password_hash, api_key, balance \
        \FROM merchants WHERE email = ?"
  merchants <- liftIO $ query conn sqlQuery [loginEmail req]

  case merchants of
    [merchant] -> do
      let passwordValid =
            validatePassword
              (encodeUtf8 $ merchantPasswordHash merchant)
              (encodeUtf8 $ loginPassword req)

      if passwordValid
        then do
          jwtSettings <- asks jwtCfg
          now <- liftIO getCurrentTime
          let expiry = addUTCTime (secondsToNominalDiffTime (3600 * 24)) now
          mJwt <- liftIO $ makeJWT merchant jwtSettings (Just expiry)

          case mJwt of
            Left _ -> throwError err500 { errBody = "Server error: Could not create access token." }
            Right jwtBytes -> do
              xsrfToken <- liftIO generateXsrfToken
              return LoginResponse
                { lrAccessToken = decodeUtf8 (L.toStrict jwtBytes)
                , lrXsrfToken   = xsrfToken
                , lrMerchant    = toPublicMerchant merchant
                }
        else throwError err401 { errBody = "Invalid email or password." }
    _ -> throwError err401 { errBody = "Invalid email or password." }

-- | Logs out the merchant by verifying they are authenticated.
-- No state is stored, so logout is simply a no-op after authentication.
logoutHandler :: AuthResult Merchant -> App NoContent
logoutHandler (Authenticated _) = return NoContent
logoutHandler _                 = throwError err401
