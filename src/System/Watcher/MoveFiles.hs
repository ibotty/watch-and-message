{-# LANGUAGE ViewPatterns #-}
module System.Watcher.MoveFiles
  ( moveDirectory
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Filesystem.Path.CurrentOS (encodeString)
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import System.Directory (createDirectory, copyFile, renameDirectory
                        , removeFile , getDirectoryContents, renameFile)
import System.FilePath ((</>), replaceDirectory)
import System.IO.Error (catchIOError, ioeGetErrorType)

import qualified Turtle

moveDirectory :: MonadIO io => Turtle.FilePath -> Turtle.FilePath -> io ()
moveDirectory src dst = liftIO $
    renameDirectory src' dst' `catchUnsupported` renameDirectory' src' dst'
  where
    src' = encodeString src
    dst' = encodeString dst

renameDirectory' :: FilePath -> FilePath -> IO ()
renameDirectory' src' dst' = do
    createDirectory dst' `catchUnsupported` return ()
    files <- map (src' </>). filter f <$> getDirectoryContents src'
    traverse_ (flip moveToDir dst') files
  where
    f n = n /= "." && n /= ".."

catchUnsupported :: IO a -> IO a -> IO a
catchUnsupported action ifFailedAction =
    catchIOError action handler
  where
    handler (ioeGetErrorType -> UnsupportedOperation) = ifFailedAction
    handler ioe                                       = ioError ioe

moveToDir :: FilePath -> FilePath -> IO ()
moveToDir src dst =
    catchUnsupported (renameFile src dstFile) $ do
        copyFile src dstFile
        removeFile src
  where
    dstFile = replaceDirectory src dst
