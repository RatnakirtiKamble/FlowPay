-- src/Handlers/AuthHandler.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handlers.AuthHandler (registerHandler, loginHandler) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text)
import qualified Data.Text as T (unpack) -- Import the unpack function with a qualifier
import Database.PostgreSQL.Simple (query)
import Servant
import Servant.Auth.Server (acceptLogin)
import Web.Cookie (SetCookie)

import App (App, AppEnv(..))
import Models.User
    ( RegistrationRequest(..), LoginRequest(..), User(..), PublicUser, toPublicUser )

-- | Handles user registration.
registerHandler :: RegistrationRequest -> App PublicUser
registerHandler req = do
  -- Hash the user's password with bcrypt, converting Text to String first.
  mHashedPasswordBytes <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (pack . T.unpack $ reqPassword req)

  case mHashedPasswordBytes of
    Nothing -> throwError err500 { errBody = "Server error: Password hashing failed." }
    Just hashedPasswordBytes -> do
      let hashedPassword = unpack hashedPasswordBytes
      conn <- asks dbConnection
      -- Insert the new user into the database
      [newUser] <- liftIO $ query conn
        "INSERT INTO users (name, email, password_hash) VALUES (?, ?, ?) RETURNING id, name, email, password_hash, balance"
        (reqName req, reqEmail req, hashedPassword)

      return $ toPublicUser newUser

-- | Handles user login.
-- | Corrected the type to expect two SetCookie headers.
loginHandler :: LoginRequest -> App (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
loginHandler req = do
  conn <- asks dbConnection
  -- Find user by email
  users <- liftIO $ query conn "SELECT * FROM users WHERE email = ?" [loginEmail req]

  case users of
    -- User found
    [user] -> do
      -- Validate the provided password against the stored hash by converting Text to String
      let passwordValid = validatePassword (pack . T.unpack $ userPasswordHash user) (pack . T.unpack $ loginPassword req)
      if passwordValid
        then do
          -- If valid, create a session cookie (JWT)
          cookieSettings <- asks cookieCfg
          jwtSettings <- asks jwtCfg
          mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
          case mApplyCookies of
            Nothing -> throwError err401
            Just applyCookies -> return $ applyCookies NoContent
        else
          throwError err401 { errBody = "Invalid email or password." }
    -- User not found
    _ -> throwError err401 { errBody = "Invalid email or password." }
