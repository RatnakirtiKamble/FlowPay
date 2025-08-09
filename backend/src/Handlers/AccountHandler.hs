-- src/Handlers/AccountHandler.hs
{-# LANGUAGE OverloadedStrings #-}

module Handlers.AccountHandler (balanceHandler) where

import Servant (throwError, err401) -- Import error handling
import Servant.Auth.Server (AuthResult(Authenticated))

import App (App)
import Models.User (User(..), BalanceResponse(..))

-- | Handles the balance check.
-- | The 'AuthResult User' is provided by the servant-auth middleware.
balanceHandler :: AuthResult User -> App BalanceResponse
balanceHandler (Authenticated user) = do
  -- The user is already authenticated, so we can just use their data.
  return $ BalanceResponse { currentBalance = userBalance user }

-- If the request is not authenticated for any reason, throw a 401 error.
-- This is much safer than calling `error`.
balanceHandler _ = throwError err401
