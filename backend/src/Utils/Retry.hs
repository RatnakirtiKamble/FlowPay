{-# LANGUAGE ScopedTypeVariables #-}

module Utils.Retry (retryWithLimit) where

import Control.Exception (SomeException, try)
import Control.Concurrent (threadDelay)

-- | Retry an IO action up to 'maxRetries' times with delay (in microseconds) between attempts
retryWithLimit :: Int -> Int -> IO a -> IO (Either SomeException a)
retryWithLimit maxRetries delayMicros action = go maxRetries
  where
    go 0 = try action  -- last try, return result or exception
    go n = do
      result <- try action
      case result of
        Right val -> return $ Right val
        Left (_ :: SomeException) -> do
          threadDelay delayMicros
          go (n - 1)
