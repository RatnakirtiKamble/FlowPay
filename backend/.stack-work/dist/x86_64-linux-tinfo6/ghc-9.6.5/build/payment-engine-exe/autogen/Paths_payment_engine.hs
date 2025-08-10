{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_payment_engine (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/bin"
libdir     = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/lib/x86_64-linux-ghc-9.6.5/payment-engine-0.1.0.0-B5qx37OXaZEGst3Vd9DGC2-payment-engine-exe"
dynlibdir  = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/lib/x86_64-linux-ghc-9.6.5"
datadir    = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/share/x86_64-linux-ghc-9.6.5/payment-engine-0.1.0.0"
libexecdir = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/libexec/x86_64-linux-ghc-9.6.5/payment-engine-0.1.0.0"
sysconfdir = "/home/ratnakirti/Work/FlowPay/backend/.stack-work/install/x86_64-linux-tinfo6/32a8d3ee08df331a7d1c44b582b02d256c773f962fa095e77707cd062a1c48c3/9.6.5/etc"

getBinDir     = catchIO (getEnv "payment_engine_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "payment_engine_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "payment_engine_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "payment_engine_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "payment_engine_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "payment_engine_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
