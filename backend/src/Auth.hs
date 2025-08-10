{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Module      : Auth
-- Description : Defines authentication types and middleware for Servant.
--
-- This module provides:
--  * Type aliases and imports for JWT and Cookie-based authentication using Servant.
--  * The 'AuthMiddleware' type representing authentication via JWT and cookies,
--    applied to the 'Merchant' user type.
--
-- It also imports key functions and types for configuring JWT and cookie settings,
-- such as 'generateKey', 'defaultJWTSettings', and 'defaultCookieSettings'.
--
module Auth where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.Auth.Server
    ( FromJWT, ToJWT, JWTSettings, CookieSettings, defaultCookieSettings,
      defaultJWTSettings, generateKey, Auth, JWT, Cookie )
import Crypto.JOSE.JWK (JWK)

import Models.Merchant (Merchant)

-- | Authentication middleware combining JWT and Cookie-based authentication
-- for the 'Merchant' user type.
type AuthMiddleware = Auth '[JWT, Cookie] Merchant
