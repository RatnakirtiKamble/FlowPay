{-# LANGUAGE OverloadedStrings #-}

module Handlers.AccountHandler (balanceHandler) where

import Servant (throwError, err401)
import Servant.Auth.Server (AuthResult(Authenticated))

import App (App)
import Models.Merchant (Merchant(..), BalanceResponse(..))

-- | Handle balance check for authenticated merchant.
balanceHandler :: AuthResult Merchant -> App BalanceResponse
balanceHandler (Authenticated merchant) = do
  return $ BalanceResponse { currentBalance = merchantBalance merchant }

-- Unauthorized access returns 401
balanceHandler _ = throwError err401
