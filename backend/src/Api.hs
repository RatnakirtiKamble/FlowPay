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
import Servant.Auth.Server (AuthResult(..), JWT, Auth)
import Data.Text (Text)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Database.PostgreSQL.Simple (query, Only(..))

import App (App, AppEnv(..))
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
  , LoginResponse
  )
import Models.Payment (Payment, PaymentRequest, PaymentResponse)


-- | API endpoint for processing payments.
type PaymentAPI =
  Header "X-API-Key" Text :> "payments" :> ReqBody '[JSON] PaymentRequest :> Post '[JSON] PaymentResponse

-- | Public API routes available without authentication.
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicMerchant
  :<|> "login"    :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
  :<|> PaymentAPI

-- | Protected API routes requiring authentication via 'AuthMiddleware'.
type ProtectedMerchantAPI =
       Auth '[JWT] Merchant :> "merchant" :> "balance"  :> Get '[JSON] BalanceResponse
  :<|> Auth '[JWT] Merchant :> "merchant" :> "apikey"   :> "generate" :> Post '[JSON] ApiKeyResponse
  :<|> Auth '[JWT] Merchant :> "merchant" :> "apikey"   :> "revoke" :> Post '[JSON] NoContent
  :<|> Auth '[JWT] Merchant :> "logout"                 :> Post '[JSON] NoContent
  :<|> Auth '[JWT] Merchant :> "merchant" :> "payments" :> Get '[JSON] [Payment]
  :<|> Auth '[JWT] Merchant :> "dashboard"              :> Get '[JSON] PublicMerchant


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
    dashboardServer (Authenticated merchant) = do
      let mId = merchantId merchant
      conn <- asks dbConnection
      let sqlQuery = "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE merchant_id = ?"

      merchants <- liftIO $ query conn sqlQuery (Only mId)
      case merchants of
        []    -> throwError err404 { errBody = "Authenticated merchant not found in database." }
        (m:_) -> return $ toPublicMerchant m 

    dashboardServer _ = throwError err401
