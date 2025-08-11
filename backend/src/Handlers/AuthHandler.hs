-- Handlers/AuthHandler.hs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handlers.AuthHandler (registerHandler, loginHandler, logoutHandler) where

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
import System.Random(randoms, getStdGen)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64

import App (App, AppEnv(..))
import Models.Merchant

-- registerHandler remains the same
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

-- ++ NEW: Helper function to generate a random string for the XSRF token
generateXsrfToken :: IO Text
generateXsrfToken = decodeUtf8 . B64.encode . B8.pack . take 32 . randoms <$> getStdGen

-- ++ NEW: loginHandler rewritten for token-based auth
loginHandler :: LoginRequest -> App LoginResponse
loginHandler req = do
  conn <- asks dbConnection
  let sqlQuery = "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE email = ?"
  merchants <- liftIO $ query conn sqlQuery [loginEmail req]

  case merchants of
    [merchant] -> do
      let passwordValid = validatePassword (encodeUtf8 $ merchantPasswordHash merchant) (encodeUtf8 $ loginPassword req)
      if passwordValid
        then do
          jwtSettings <- asks jwtCfg

          -- ++ NEW: Get the current time and calculate the expiry timestamp
          now <- liftIO getCurrentTime
          let expiry = addUTCTime (secondsToNominalDiffTime (3600 * 24)) now -- 24 hours from now

          -- Manually create the JWT, passing the calculated expiry time
          mJwt <- liftIO $ makeJWT merchant jwtSettings (Just expiry)

          case mJwt of
            Left _ -> throwError err500 { errBody = "Server error: Could not create access token." }
            Right jwtBytes -> do
              -- Generate a new XSRF token
              xsrfToken <- liftIO generateXsrfToken

              return LoginResponse
                { lrAccessToken = decodeUtf8 (L.toStrict jwtBytes)
                , lrXsrfToken   = xsrfToken
                , lrMerchant    = toPublicMerchant merchant
                }
        else throwError err401 { errBody = "Invalid email or password." }
    _ -> throwError err401 { errBody = "Invalid email or password." }


-- ++ NEW: logoutHandler is now much simpler.
-- It authenticates the user to ensure they are logged in, but does nothing else.
logoutHandler :: AuthResult Merchant -> App NoContent
logoutHandler (Authenticated _) = return NoContent
logoutHandler _                 = throwError err401