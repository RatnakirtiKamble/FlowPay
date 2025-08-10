{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- Module      : Handlers.AuthHandler
-- Description : Handlers for public authentication routes: registration, login, and logout.
--
-- This module provides handlers for merchants to:
--  * Register a new account with hashed password storage
--  * Login by validating credentials and issuing JWT and XSRF cookies
--  * Logout by expiring authentication cookies
--
-- Passwords are hashed securely using bcrypt before storage.
-- Login establishes an authenticated session via cookies.
-- Logout clears these cookies to invalidate the session on the client side.
--
module Handlers.AuthHandler (registerHandler, loginHandler, logoutHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime(..), Day(..))
import Database.PostgreSQL.Simple (query)
import Servant
import Servant.Auth.Server (AuthResult(..), acceptLogin)
import Web.Cookie (SetCookie, defaultSetCookie, setCookieName, setCookieValue, setCookieExpires, setCookiePath)

import App (App, AppEnv(..))
import Models.Merchant
  ( RegistrationRequest(..)
  , LoginRequest(..)
  , Merchant(..)
  , PublicMerchant(..)
  , toPublicMerchant
  )

-- | Registers a new merchant user.
--
-- Takes a 'RegistrationRequest' with user details and plaintext password,
-- hashes the password using bcrypt, stores the new merchant in the database,
-- and returns the public merchant information (without sensitive data).
--
-- Throws HTTP 500 error if password hashing fails.
registerHandler :: RegistrationRequest -> App PublicMerchant
registerHandler req = do
  mHashedPasswordBytes <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 $ reqPassword req)
  case mHashedPasswordBytes of
    Nothing -> throwError err500 { errBody = "Server error: Password hashing failed." }
    Just hashedPasswordBytes -> do
      let hashedPassword = decodeUtf8 hashedPasswordBytes
      conn <- asks dbConnection
      [newMerchant] <- liftIO $ query conn
        "INSERT INTO merchants (name, email, password_hash, balance) VALUES (?, ?, ?, 0) RETURNING merchant_id, name, email, password_hash, api_key, balance"
        (reqName req, reqEmail req, hashedPassword)
      return $ toPublicMerchant newMerchant

-- | Authenticates a merchant by validating email and password.
--
-- On success, returns HTTP headers to set JWT and XSRF cookies to establish
-- an authenticated session.
--
-- Returns HTTP 401 Unauthorized if credentials are invalid or login fails.
loginHandler :: LoginRequest -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler req = do
  conn <- asks dbConnection
  let sqlQuery = "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE email = ?"
  merchants <- liftIO $ query conn sqlQuery [loginEmail req]

  case merchants of
    [merchant] -> do
      let passwordValid = validatePassword (encodeUtf8 $ merchantPasswordHash merchant) (encodeUtf8 $ loginPassword req)
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

-- | Creates a 'SetCookie' header that expires and deletes a cookie immediately.
--
-- Used to instruct browsers to remove authentication cookies during logout.
expireCookie :: ByteString -> SetCookie
expireCookie name = defaultSetCookie
  { setCookieName = name
  , setCookieValue = ""
  , setCookiePath = Just "/"
  , setCookieExpires = Just (UTCTime (ModifiedJulianDay 0) 0) -- A timestamp far in the past
  }

-- | Logs out an authenticated merchant by expiring the JWT and XSRF cookies.
--
-- Returns HTTP 204 No Content with 'Set-Cookie' headers that instruct
-- the client to delete these cookies.
--
-- Returns HTTP 401 Unauthorized if authentication fails.
logoutHandler :: AuthResult Merchant -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logoutHandler (Authenticated _) = do
  let expiredJwtCookie = expireCookie "JWT-Cookie"
  let expiredXsrfCookie = expireCookie "XSRF-TOKEN"
  return $ addHeader expiredJwtCookie $ addHeader expiredXsrfCookie NoContent
logoutHandler _ = throwError err401
