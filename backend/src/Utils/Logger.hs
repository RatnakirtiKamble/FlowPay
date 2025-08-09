{-# LANGUAGE OverloadedStrings #-}

module Utils.Logger (logInfo, logError) where

import Data.Time.Clock (getCurrentTime)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO


logInfo :: Text -> IO ()
logInfo msg = do
    now <- getCurrentTime
    TIO.putStrLn $ "[" <> (pack $ show now) <> "] INFO: " <> msg

logError :: Text -> IO ()
logError msg = do
    now <- getCurrentTime
    TIO.putStrLn $ "[" <> (pack $ show now) <> "] ERROR: " <> msg


