{-# LANGUAGE TupleSections #-}
module System.Watcher.WatchFiles
  ( monitorWatches
  , watchWriteClosedFilesInThread
  , threaded
  , addWatch'
  , getRelPath
  , ) where

import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Data.Foldable (traverse_)

import qualified System.Linux.Inotify as Inotify
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Turtle

-- | Watch for 'Inotify.in_CLOSE_WRITE' events in the given
-- 'Turtle.FilePath's.
watchWriteClosedFilesInThread
  :: [Turtle.FilePath] -> (Turtle.FilePath -> IO ()) -> IO ()
watchWriteClosedFilesInThread files action =
    monitorWatches watches (threaded (action . getRelPath))
  where
    watches = map (, Inotify.in_CLOSE_WRITE) files

monitorWatches :: [(Turtle.FilePath, Inotify.Mask Inotify.WatchFlag)]
               -> (Inotify.Event -> IO ())
               -> IO ()
monitorWatches watches action = do
    inotify <- Inotify.init
    traverse_ (uncurry (addWatch' inotify)) watches
    forever $ Inotify.getEvent inotify >>= action

-- | Spawns a new thread to process the 'Inotify.Event'.
threaded :: (a -> IO ()) -> a -> IO ()
threaded action = void . forkIO . action

addWatch' :: Inotify.Inotify
          -> Turtle.FilePath
          -> Inotify.Mask Inotify.WatchFlag
          -> IO Inotify.Watch
addWatch' inotify file =
    Inotify.addWatch inotify (show file)

getRelPath :: Inotify.Event -> Turtle.FilePath
getRelPath = FilePath.decode . Inotify.name
