{-# LANGUAGE CPP #-}
module GetTempDir
  where
import           System.Directory  (getTemporaryDirectory)

#ifndef WINDOWS
getTempDir :: IO (FilePath)
getTempDir = getTemporaryDirectory
#else
getTempDir :: IO (FilePath)
getTempDir = do
  tmp <- getTemporaryDirectory
  return $ tmp ++ "/"
#endif
