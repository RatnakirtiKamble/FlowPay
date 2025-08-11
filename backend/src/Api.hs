{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Api
-- Description : Defines the Servant API type and server implementation.
--
-- This module exposes the complete API type for the application, including:
--  * Public routes: registration, login, and payment processing
--  * Protected routes: merchant balance, API key management, logout,
--    payments listing, and merchant dashboard
--
-- The API uses authentication middleware to protect sensitive endpoints.
-- The server implementation wires up all handlers imported from the Handlers modules.
--
module Api (api, server) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult(..))
import Web.Cookie (SetCookie)
import Data.Text (Text, pack)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Database.PostgreSQL.Simple (query, Only(..))
import Control.Monad.Logger (logInfoN)

import App (App, AppEnv(..))
import Auth (AuthMiddleware)
import Handlers.AuthHandler (registerHandler, loginHandler, logoutHandler)
import Handlers.AccountHandler (balanceHandler)
import Handlers.PaymentHandler (paymentServer)
import Handlers.MerchantHandler (merchantServer, revokeApiKeyHandler, listPaymentsHandler, ApiKeyResponse(..))

import Models.Merchant
  ( RegistrationRequest
  , LoginRequest
  , BalanceResponse
  , Merchant(merchantId)
  , PublicMerchant
  , toPublicMerchant
  )
import Models.Payment (Payment, PaymentRequest, PaymentResponse)


-- | API endpoint for processing payments.
type PaymentAPI =
  Header "X-API-Key" Text :> "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse

-- | Public API routes available without authentication.
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicMerchant
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> PaymentAPI

-- | Protected API routes requiring authentication via 'AuthMiddleware'.
type ProtectedMerchantAPI =
       AuthMiddleware :> "merchant" :> "balance" :> Get '[JSON] BalanceResponse
  :<|> AuthMiddleware :> "merchant" :> "apikey" :> "generate" :> Post '[JSON] ApiKeyResponse
  :<|> AuthMiddleware :> "merchant" :> "apikey" :> "revoke" :> Post '[JSON] NoContent
  :<|> AuthMiddleware :> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> AuthMiddleware :> "merchant" :> "payments" :> Get '[JSON] [Payment] 
  :<|> AuthMiddleware :> "dashboard" :> Get '[JSON] PublicMerchant

type ProtectedAPI = ProtectedMerchantAPI

-- | Complete API type combining public and protected routes.
type API = PublicAPI :<|> ProtectedAPI

-- | Proxy for the API type.
api :: Proxy API
api = Proxy

-- | Server implementation for the full API.
--
-- Combines the public and protected servers by wiring up all handler functions.
server :: ServerT API App
server = publicServer :<|> protectedServer
  where
    -- Public endpoints
    publicServer = registerHandler :<|> loginHandler :<|> paymentServer

    -- Protected endpoints
    protectedServer =
             merchantBalanceServer
        :<|> merchantApiKeyServer
        :<|> revokeApiKeyHandler
        :<|> logoutHandler
        :<|> listPaymentsHandler
        :<|> dashboardServer

    merchantBalanceServer = balanceHandler
    merchantApiKeyServer = merchantServer
    logoutHandler = Handlers.AuthHandler.logoutHandler
    listPaymentsHandler = Handlers.MerchantHandler.listPaymentsHandler

    -- | Returns fresh public merchant details for the authenticated merchant.
    dashboardServer :: AuthResult Merchant -> App PublicMerchant
    dashboardServer authResult = do


      -- Log to your monad logger (which writes to app.log)
      logInfoN $ "[DEBUG] dashboardServer reached with AuthResult: " <> pack (show authResult)
      case authResult of
        Authenticated merchant -> do
          let mId = merchantId merchant
          conn <- asks dbConnection
          let sqlQuery = "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE merchant_id = ?"
          merchants <- liftIO $ query conn sqlQuery (Only mId)
          case merchants of
            []    -> throwError err404 { errBody = "Authenticated merchant not found in database." }
            (m:_) -> return $ toPublicMerchant m
        _ -> throwError err401
