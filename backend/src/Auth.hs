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

import Models.Merchant (Merchant)

type AuthMiddleware = Auth '[JWT, Cookie] Merchant
