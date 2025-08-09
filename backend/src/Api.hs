-- src/Api.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (api, server) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult, Auth, Cookie)
import Web.Cookie (SetCookie)

import App (App)
import Auth (AuthMiddleware)
import Handlers.AuthHandler (registerHandler, loginHandler)
import Handlers.AccountHandler (balanceHandler)
import Handlers.PaymentHandler (paymentServer)
import Handlers.UserHandler (userServer)
import Models.User
    ( RegistrationRequest, LoginRequest, PublicUser, BalanceResponse, User )
import Models.Payment (PaymentRequest, PaymentResponse)

-- | Public routes that do not require authentication.
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicUser
  -- Corrected the type to expect two Set-Cookie headers
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

-- | Protected routes that require a valid JWT cookie.
type ProtectedAPI =
  Auth '[Cookie] User :> "account" :> "balance" :> Get '[JSON] BalanceResponse

-- | The complete API, combining public and protected routes.
type API = PublicAPI :<|> ProtectedAPI

-- | API proxy.
api :: Proxy API
api = Proxy

-- | Server combining all handlers.
server :: ServerT API App
server = publicServer :<|> protectedServer
  where
    publicServer = registerHandler :<|> loginHandler
    protectedServer = balanceHandler