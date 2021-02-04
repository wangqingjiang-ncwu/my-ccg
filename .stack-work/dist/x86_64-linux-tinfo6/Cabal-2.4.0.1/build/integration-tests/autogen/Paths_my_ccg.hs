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
version = Version [0,1,1,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/bin"
libdir     = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/lib/x86_64-linux-ghc-8.6.4/my-ccg-0.1.1.0-3jsQMrt7A9J5bjfjqmLoOX-integration-tests"
dynlibdir  = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/lib/x86_64-linux-ghc-8.6.4"
datadir    = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/share/x86_64-linux-ghc-8.6.4/my-ccg-0.1.1.0"
libexecdir = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/libexec/x86_64-linux-ghc-8.6.4/my-ccg-0.1.1.0"
sysconfdir = "/home/qingjiangwang/my-ccg/.stack-work/install/x86_64-linux-tinfo6/04430424fd9cbb9d87302bccaa830ceb49b1d36f8e6472ca7ecc5fa3ebe5dd56/8.6.4/etc"

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
  return (dir ++ "/" ++ name)
