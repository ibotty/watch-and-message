{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module System.Watcher.WatchFiles
  ( WatchDescriptorNotFound
  , monitorWatches
  , watchWriteClosedFilesInThread
  , threaded
  , addWatch'
  , getRelPath
  , ) where

import Control.Concurrent.Lifted (fork)
import Control.Exception (Exception, displayException)
import Control.Monad (forever, void)
import Control.Monad.Base (liftBase)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Map (Map)
import System.Watcher.Types
import System.Watcher.Log
import Turtle (format, w)

import qualified Data.Map as Map
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified System.Linux.Inotify as Inotify
import qualified Turtle

data WatchDescriptorNotFound = WatchDescriptorNotFound
  deriving (Show, Eq, Read)

instance Exception WatchDescriptorNotFound where
    displayException _ = "Imposible exception: Cannot find watch descriptor in map."


-- | Watch for 'Inotify.in_CLOSE_WRITE' events in the given
-- 'Turtle.FilePath's.
watchWriteClosedFilesInThread
  :: (MonadLog (WithSeverity LogEntry) io, MonadThrow io, MonadBaseControl IO io)
  => [MoveConfig] -> (MoveConfig -> Turtle.FilePath -> io ()) -> io ()
watchWriteClosedFilesInThread moveConfigs action = do
    logDebug $ LogEntry "Watches" [("dirs", format w (map dirToWatch moveConfigs))]
    monitorWatches watchDefs (curry (threaded action'))
  where
    watchDefs = map (\m -> ((dirToWatch m, Inotify.in_CLOSE_WRITE), m)) moveConfigs
    action' (watchMap, event) = do
      a <- lookupWatch (Inotify.wd event) watchMap
      action a (getRelPath event)

lookupWatch :: (MonadThrow m, Ord k) => k -> Map k v -> m v
lookupWatch k = maybe (throwM WatchDescriptorNotFound) return . Map.lookup k

monitorWatches
  :: MonadBaseControl IO io
  => [((Turtle.FilePath, Inotify.Mask Inotify.WatchFlag), a)]
  -> (Map Inotify.Watch a -> Inotify.Event -> io ())
  -> io ()
monitorWatches watchDefs action = do
    inotify <- liftBase Inotify.init
    watches <- traverse (uncurry (addWatch' inotify)) $ map fst watchDefs
    let watchMap = Map.fromList . zip watches $ map snd watchDefs
    forever $ liftBase (Inotify.getEvent inotify) >>= action watchMap

-- | Spawns a new thread to process the 'Inotify.Event'.
threaded :: MonadBaseControl IO io => (a -> io ()) -> a -> io ()
threaded action = void . fork . action

addWatch' :: MonadBaseControl IO io
          => Inotify.Inotify
          -> Turtle.FilePath
          -> Inotify.Mask Inotify.WatchFlag
          -> io Inotify.Watch
addWatch' inotify file = liftBase . Inotify.addWatch inotify file'
  where
    file' = FilePath.encodeString file

getRelPath :: Inotify.Event -> Turtle.FilePath
getRelPath = FilePath.decode . Inotify.name
