{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_my_ccg (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,2,5,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\bin"
libdir     = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\lib\\x86_64-windows-ghc-8.6.4\\my-ccg-0.2.5.1-JaJnIz8axN0Hb5LkFfXk0i-my-ccg-test"
dynlibdir  = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\lib\\x86_64-windows-ghc-8.6.4"
datadir    = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\share\\x86_64-windows-ghc-8.6.4\\my-ccg-0.2.5.1"
libexecdir = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\libexec\\x86_64-windows-ghc-8.6.4\\my-ccg-0.2.5.1"
sysconfdir = "D:\\GitHub\\my-ccg\\.stack-work\\install\\52201ef5\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "my_ccg_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_ccg_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "my_ccg_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "my_ccg_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_ccg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "my_ccg_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
