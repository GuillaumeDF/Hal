{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_hal_project (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/bin"
libdir     = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/lib/x86_64-osx-ghc-8.6.5/hal-project-0.1.0.0-Ll0tSMEYZ3d1x5IueRyn0Q"
dynlibdir  = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/share/x86_64-osx-ghc-8.6.5/hal-project-0.1.0.0"
libexecdir = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/libexec/x86_64-osx-ghc-8.6.5/hal-project-0.1.0.0"
sysconfdir = "/Users/guillaume/Documents/Epitech/FUN_hal_2019/.stack-work/install/x86_64-osx/9227a2850aaf5d439d7f963f2bdc73d6312bcb077108856d2583e8c38c8b9fad/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hal_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hal_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "hal_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "hal_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hal_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hal_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
