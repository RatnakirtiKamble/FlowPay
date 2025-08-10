{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- |
-- This module contains the handlers for public-facing authentication routes,
-- including user registration, login, and logout.
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

-- |
-- Handles new merchant registration. Hashes the provided password and inserts
-- a new merchant record into the database, returning the public details of the
-- newly created merchant.
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

-- |
-- Handles merchant login. It validates the provided email and password against
-- the database. On success, it generates JWT and XSRF cookies and returns them
-- in the response headers to establish a session.
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

-- |
-- Creates a 'SetCookie' header that instructs the browser to immediately
-- expire and delete a cookie by setting its expiration date to a time in the past.
expireCookie :: ByteString -> SetCookie
expireCookie name = defaultSetCookie
  { setCookieName = name
  , setCookieValue = ""
  , setCookiePath = Just "/"
  , setCookieExpires = Just (UTCTime (ModifiedJulianDay 0) 0) -- A time in the distant past
  }

-- |
-- Handles merchant logout. This is a protected endpoint that, upon successful
-- authentication, returns 'Set-Cookie' headers that expire the JWT and XSRF
-- cookies, effectively logging the user out on the client-side.
logoutHandler :: AuthResult Merchant -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
logoutHandler (Authenticated _) = do
  let expiredJwtCookie = expireCookie "JWT-Cookie"
  let expiredXsrfCookie = expireCookie "XSRF-TOKEN"
  return $ addHeader expiredJwtCookie $ addHeader expiredXsrfCookie NoContent
logoutHandler _ = throwError err401
