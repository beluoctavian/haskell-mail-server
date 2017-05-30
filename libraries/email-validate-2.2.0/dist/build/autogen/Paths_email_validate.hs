module Paths_email_validate (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [2,2,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/email-validate-2.2.0/ghc-7.6.3"
datadir    = "/usr/local/share/email-validate-2.2.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "email_validate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "email_validate_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "email_validate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "email_validate_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
