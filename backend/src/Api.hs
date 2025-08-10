{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (api, server) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Servant
import Servant.Auth.Server (AuthResult(..))
import Web.Cookie (SetCookie)
import Data.Text (Text) -- <-- THIS IS THE FIX: Add this import back.

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Database.PostgreSQL.Simple (query)

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

-- | Public routes
type PublicAPI =
       "register" :> ReqBody '[JSON] RegistrationRequest :> Post '[JSON] PublicMerchant
  :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> PaymentAPI

-- | Protected routes
type ProtectedMerchantAPI =
       AuthMiddleware :> "merchant" :> "balance" :> Get '[JSON] BalanceResponse
  :<|> AuthMiddleware :> "merchant" :> "apikey" :> "generate" :> Post '[JSON] ApiKeyResponse
  :<|> AuthMiddleware :> "merchant" :> "apikey" :> "revoke" :> Post '[JSON] NoContent
  :<|> AuthMiddleware :> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
  :<|> AuthMiddleware :> "merchant" :> "payments" :> Get '[JSON] [Payment] 
  :<|> AuthMiddleware :> "dashboard" :> Get '[JSON] PublicMerchant

type ProtectedAPI = ProtectedMerchantAPI

type API = PublicAPI :<|> ProtectedAPI

-- | API proxy
api :: Proxy API
api = Proxy

-- | Server
server :: ServerT API App
server = publicServer :<|> protectedServer
  where
    publicServer = registerHandler :<|> loginHandler :<|> paymentServer
    protectedServer = merchantBalanceServer 
                    :<|> merchantApiKeyServer 
                    :<|> revokeApiKeyHandler
                    :<|> logoutHandler
                    :<|> listPaymentsHandler
                    :<|> dashboardServer
                    

    merchantBalanceServer = balanceHandler
    merchantApiKeyServer = merchantServer
    revokeApiKeyServer= revokeApiKeyHandler
    logoutHandler = Handlers.AuthHandler.logoutHandler 
    listPaymentsHandler = Handlers.MerchantHandler.listPaymentsHandler

    dashboardServer :: AuthResult Merchant -> App PublicMerchant
    dashboardServer (Authenticated merchant) = do
      let mId = merchantId merchant
      conn <- asks dbConnection
      let sqlQuery = "SELECT merchant_id, name, email, password_hash, api_key, balance FROM merchants WHERE merchant_id = ?"
      [freshMerchant] <- liftIO $ query conn sqlQuery [mId]
      return $ toPublicMerchant freshMerchant
    dashboardServer _ = throwError err401