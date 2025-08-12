{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Api
Description : Defines the Servant API type and server implementation.

This module contains the full application API definition and its corresponding
server implementation.

It includes:
  * Public routes: registration, login, and payment processing.
  * Protected routes: merchant balance, API key management, logout, payment history, and dashboard.

Authentication:
  * Public routes are accessible without authentication.
  * Protected routes require JWT-based authentication via `Servant.Auth.Server`.

Handlers are imported from dedicated `Handlers.*` modules to keep concerns separated.
-}
module Api (api, server) where

-- ========== Imports ==========
import Data.Text (Text)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult(..), JWT, Auth)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Database.PostgreSQL.Simple (query, Only(..))

-- Application & Handlers
import App (App, AppEnv(..))
import Handlers.AuthHandler (registerHandler, loginHandler, logoutHandler)
import Handlers.AccountHandler (balanceHandler)
import Handlers.PaymentHandler (paymentServer)
import Handlers.MerchantHandler
  ( merchantServer
  , revokeApiKeyHandler
  , listPaymentsHandler
  , ApiKeyResponse(..)
  )

-- Models
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

-- ========== API TYPE DEFINITIONS ==========

-- | Endpoint for processing payments using an API key.
type PaymentAPI =
       Header "X-API-Key" Text
  :>   "payments"
  :>   ReqBody '[JSON] PaymentRequest
  :>   Post '[JSON] PaymentResponse

-- | Public routes accessible without authentication.
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicMerchant
  :<|> "login"    :> ReqBody '[JSON] LoginRequest       :> Post '[JSON] LoginResponse
  :<|> PaymentAPI

-- | Protected merchant routes requiring JWT authentication.
type ProtectedMerchantAPI =
       Auth '[JWT] Merchant :> "merchant" :> "balance"   :> Get  '[JSON] BalanceResponse
  :<|> Auth '[JWT] Merchant :> "merchant" :> "apikey"    :> "generate" :> Post '[JSON] ApiKeyResponse
  :<|> Auth '[JWT] Merchant :> "merchant" :> "apikey"    :> "revoke"   :> Post '[JSON] NoContent
  :<|> Auth '[JWT] Merchant :> "logout"                  :> Post '[JSON] NoContent
  :<|> Auth '[JWT] Merchant :> "merchant" :> "payments"  :> Get  '[JSON] [Payment]
  :<|> Auth '[JWT] Merchant :> "dashboard"               :> Get  '[JSON] PublicMerchant

type ProtectedAPI = ProtectedMerchantAPI

-- | Full API: public + protected.
type API = PublicAPI :<|> ProtectedAPI

-- | Proxy for API type.
api :: Proxy API
api = Proxy

-- ========== SERVER IMPLEMENTATION ==========

-- | Server implementation wiring up public and protected routes.
server :: ServerT API App
server = publicServer :<|> protectedServer
  where
    -- ----- Public Routes -----
    publicServer =
           registerHandler
      :<|> loginHandler
      :<|> paymentServer

    -- ----- Protected Routes -----
    protectedServer =
           balanceHandler
      :<|> merchantServer
      :<|> revokeApiKeyHandler
      :<|> logoutHandler
      :<|> listPaymentsHandler
      :<|> dashboardServer

    -- | Fetch fresh merchant details for authenticated user.
    dashboardServer :: AuthResult Merchant -> App PublicMerchant
    dashboardServer (Authenticated merchant) = do
      let mId = merchantId merchant
      conn <- asks dbConnection
      let sql = "SELECT merchant_id, name, email, password_hash, api_key, balance \
                \FROM merchants WHERE merchant_id = ?"
      merchants <- liftIO $ query conn sql (Only mId)
      case merchants of
        []    -> throwError err404 { errBody = "Authenticated merchant not found in database." }
        (m:_) -> return $ toPublicMerchant m
    dashboardServer _ = throwError err401
