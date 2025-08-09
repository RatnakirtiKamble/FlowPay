{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (api, server) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult)
import Web.Cookie (SetCookie)
import Data.Text (Text)

import App (App)
import Auth (AuthMiddleware)
import Handlers.AuthHandler (registerHandler, loginHandler)
import Handlers.AccountHandler (balanceHandler)
import Handlers.PaymentHandler (paymentServer)
import Handlers.MerchantHandler (merchantServer)
-- no UserHandler import now

import Models.Merchant
  ( RegistrationRequest
  , LoginRequest
  , PublicMerchant
  , BalanceResponse
  )
import Models.Payment (PaymentRequest, PaymentResponse)

-- | API endpoint for processing payments.
type PaymentAPI = 
  Header "X-API-Key" Text :> "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse

-- | Public routes 
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicMerchant
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> PaymentAPI  -- Payment endpoint public (API key inside req body or header)

-- | Protected routes
type ProtectedMerchantAPI =
       AuthMiddleware :> "merchant" :> "balance" :> Get '[JSON] BalanceResponse
  :<|> AuthMiddleware :> "merchant" :> "apikey" :> "generate" :> Post '[JSON] Text


type ProtectedAPI = ProtectedMerchantAPI


type API = PublicAPI :<|> ProtectedAPI

-- | API proxy
api :: Proxy API
api = Proxy

-- | Server
server :: ServerT API App
server = publicServer :<|> protectedServer
  where
    -- Public routes
    publicServer = registerHandler :<|> loginHandler :<|> paymentServer

    -- Protected merchant routes
    protectedServer = merchantBalanceServer :<|> merchantApiKeyServer

    merchantBalanceServer = balanceHandler
    merchantApiKeyServer = merchantServer
