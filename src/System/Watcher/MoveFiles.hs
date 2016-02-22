module System.Watcher.MoveFiles
  ( moveDir
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Filesystem.Path.CurrentOS (encodeString)
import System.Directory (createDirectory, getDirectoryContents, renameFile)
import System.FilePath ((</>), replaceDirectory)
import System.IO.Error (catchIOError)

import qualified Turtle

moveDir :: MonadIO io => Turtle.FilePath -> Turtle.FilePath -> io ()
moveDir src dst = liftIO $ do
    catchIOError (createDirectory dst') (const $ return ()) -- ignore error
    traverse_ (flip moveToDir dst') =<< map (src' </>). filter f <$> getDirectoryContents src'
  where
    f n = n /= "." && n /= ".."
    src' = encodeString src
    dst' = encodeString dst

moveToDir :: FilePath -> FilePath -> IO ()
moveToDir src dst = renameFile src (replaceDirectory src dst)
