{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Handlers.AccountHandler
-- Description : Handler for retrieving the current balance of an authenticated merchant.
--
-- This module exposes a single handler that returns the merchant's current balance.
-- Access is restricted to authenticated merchants; unauthorized requests result in HTTP 401.
module Handlers.AccountHandler (balanceHandler) where

import Servant (throwError, err401)
import Servant.Auth.Server (AuthResult(Authenticated))

import App (App)
import Models.Merchant (Merchant(..), BalanceResponse(..))

-- | Returns the current balance of the authenticated merchant.
--
-- Responds with HTTP 401 Unauthorized if the request is not authenticated.
balanceHandler :: AuthResult Merchant -> App BalanceResponse
balanceHandler (Authenticated merchant) = do
  return $ BalanceResponse { currentBalance = merchantBalance merchant }

balanceHandler _ = throwError err401
