{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module System.Watcher.WatchFiles
  ( WatchDescriptorNotFound
  , WatcherState
  , initWatcher
  , threaded
  , watchManyClosedFilesRecursively
  , watchClosedFilesRecursively
  , setupWatch'
  , monitorRecursively
  , addClosedRecursiveWatch
  , getRelPath
  , ) where

import Prelude hiding (init)

import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM (STM, TVar, atomically, newTVarIO, readTVarIO, modifyTVar')
import Control.Exception (Exception, displayException)
import Control.Monad (forever, void, when)
import Control.Monad.Base (liftBase)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import System.Directory (doesDirectoryExist)
import System.Linux.Inotify
import System.Watcher.Types
import System.Watcher.Log
import Turtle ((</>), format, fp)

import qualified Data.HashMap.Strict as HashMap
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Turtle

type Action io = Turtle.FilePath -> Event -> io ()

type WatcherState io = (Inotify, TVar (HashMap Watch (Event -> io ())))

data WatchDescriptorNotFound = WatchDescriptorNotFound
  deriving (Show, Eq, Read)

instance Exception WatchDescriptorNotFound where
    displayException _ = "Imposible exception: Cannot find watch descriptor in map."

-- | initialize 'WatcherState'.
initWatcher :: MonadBaseControl IO io => io (WatcherState io)
initWatcher = liftBase $ (,) <$> init <*> newTVarIO HashMap.empty

watchManyClosedFilesRecursively
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io, MonadThrow io)
  => [(Turtle.FilePath, Turtle.FilePath -> io ())] -> io ()
watchManyClosedFilesRecursively filesActions = do
    wState <- initWatcher
    traverse_ (uncurry (addClosedRecursiveWatch wState)) filesActions
    loop wState

watchClosedFilesRecursively
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io, MonadThrow io)
  => Turtle.FilePath -> (Turtle.FilePath -> io ()) -> io ()
watchClosedFilesRecursively file action =
    watchManyClosedFilesRecursively [(file, action)]


addClosedRecursiveWatch
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io, MonadThrow io)
  => WatcherState io
  -> Turtle.FilePath
  -> (Turtle.FilePath -> io ())
  -> io ()
addClosedRecursiveWatch wState f action =
    setupWatch' wState f in_CLOSE_WRITE action'
  where
    action' base event = action (base </> getRelPath event)

lookupWatch :: (MonadThrow m, Eq k, Hashable k) => k -> HashMap k v -> m v
lookupWatch k = maybe (throwM WatchDescriptorNotFound) return . HashMap.lookup k

monitorRecursively
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io, MonadThrow io)
  => Turtle.FilePath
  -> Mask WatchFlag
  -> Action io
  -> io ()
monitorRecursively file mask' action = do
    wState <- initWatcher
    setupWatch' wState file mask' action
    loop wState

setupWatch'
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io, MonadThrow io)
  => WatcherState io
  -> Turtle.FilePath
  -> Mask WatchFlag
  -> Action io
  -> io ()
setupWatch' wState file mask' action =
    addWatch' wState file combinedMask (action >> const action')
  where
    combinedMask = mask' <> in_CREATE
    action' event = when (in_CREATE `hasOverlap` mask event) $
        addSubdirWatch wState mask' action file event

loop :: (MonadBaseControl IO io, MonadThrow io) => WatcherState io -> io ()
loop (inotify, watchRef) = forever $ liftBase (getEvent inotify) >>= action'
  where
    action' event = do
        watchMap <- liftBase $ readTVarIO watchRef
        act <- lookupWatch (wd event) watchMap
        act event

addSubdirWatch
  :: (MonadBaseControl IO io, MonadLog (WithSeverity LogEntry) io)
  => WatcherState io
  -> Mask WatchFlag
  -> Action io
  -> Turtle.FilePath -> Event -> io ()
addSubdirWatch wState mask' action directory event = do
    exists <- liftBase $ doesDirectoryExist (FilePath.encodeString file)
    when exists $ do
      logDebug $ LogEntry "addSubdirWatch" [("dir", format fp file)]
      addWatch' wState file mask' action
  where
    file = directory </> getRelPath event

-- | Spawns a new thread to process the 'Event'.
threaded :: MonadBaseControl IO io => (a -> io ()) -> a -> io ()
threaded action = void . fork . action

addWatch'
  :: MonadBaseControl IO io
  => WatcherState io -> Turtle.FilePath -> Mask WatchFlag -> Action io -> io ()
addWatch' (inotify, watchRef) file mask' action = do
    watch <- liftBase $ addWatch inotify file' mask'
    atomically' $ modifyTVar' watchRef (HashMap.insertWith (>>) watch (action file))
  where
    file' = FilePath.encodeString file

getRelPath :: Event -> Turtle.FilePath
getRelPath = FilePath.decode . name

atomically' :: MonadBaseControl IO io => STM a -> io a
atomically' = liftBase . atomically
