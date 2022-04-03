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

bindir     = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/bin"
libdir     = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/lib/x86_64-linux-ghc-8.6.5/hal-project-0.1.0.0-Ll0tSMEYZ3d1x5IueRyn0Q"
dynlibdir  = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/share/x86_64-linux-ghc-8.6.5/hal-project-0.1.0.0"
libexecdir = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/libexec/x86_64-linux-ghc-8.6.5/hal-project-0.1.0.0"
sysconfdir = "/home/student/FUN_hal_2019/.stack-work/install/x86_64-linux-tinfo6/ac31884548d96e304c8b9929bfcc42dd3d0d21c6770d862c90f6a60fb649085e/8.6.5/etc"

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
