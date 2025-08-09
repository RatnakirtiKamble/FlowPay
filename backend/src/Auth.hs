-- src/Auth.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.Auth.Server
    ( FromJWT, ToJWT, JWTSettings, CookieSettings, defaultCookieSettings,
      defaultJWTSettings, generateKey, Auth, JWT, Cookie )
import Crypto.JOSE.JWK (JWK)

import Models.User (User)

-- Note: The ToJWT and FromJWT instances for User are now correctly
-- defined in the Models.User module.

-- | Define the authentication scheme for our API.
-- | We are using a cookie-based JWT approach.
type AuthMiddleware = Auth '[JWT, Cookie] User
