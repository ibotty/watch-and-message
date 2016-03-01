{-# LANGUAGE ViewPatterns #-}
module System.Watcher.MoveFiles
  ( moveDirectory
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Filesystem.Path.CurrentOS (encodeString)
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import System.Directory (createDirectory, copyFile, removeFile, getDirectoryContents, renameFile)
import System.FilePath ((</>), replaceDirectory)
import System.IO.Error (catchIOError, ioeGetErrorType)

import qualified Turtle

moveDirectory :: MonadIO io => Turtle.FilePath -> Turtle.FilePath -> io ()
moveDirectory src dst = liftIO $ do
    catchIOError (createDirectory dst') (const $ return ()) -- ignore any error
    traverse_ (flip moveToDir dst') =<< map (src' </>). filter f <$> getDirectoryContents src'
  where
    f n = n /= "." && n /= ".."
    src' = encodeString src
    dst' = encodeString dst

moveToDir :: FilePath -> FilePath -> IO ()
moveToDir src dst =
    catchIOError (renameFile src dstFile)
                  handler

  where
    dstFile = replaceDirectory src dst
    handler (ioeGetErrorType -> UnsupportedOperation) = do
        copyFile src dstFile
        removeFile src
    handler ioe = ioError ioe
