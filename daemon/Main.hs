{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude hiding (FilePath)
import Control.Arrow ((&&&))
import Control.Exception (SomeException, displayException)
import Control.Monad (when)
import Control.Monad.Base (liftBase)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import Data.Monoid ((<>))
import Options.Applicative (ParserInfo, execParser, fullDesc, info, progDesc, helper)
import Filesystem.Path.CurrentOS (encodeString)
import System.Exit (exitSuccess, exitFailure)
import System.Directory (getDirectoryContents)
import System.IO (stderr)
import System.Watcher
import Turtle ((</>), FilePath, repr, format, fp, w)

import qualified Data.Text as Text
import qualified Filesystem.Path as Path


opts :: ParserInfo Config
opts = info (helper <*> parseConfig)
            (  fullDesc
            <> progDesc "Watch a directory and post ")

main :: IO ()
main = do
    config <- execParser opts
    runLog (loglevel config) . flip evalStateT (kafkaConfig config)
                             . flip catch logErrorHandler $ do
        traverse_ (logInfo . renderWhatToDo) $ moveConfig config
        logDebug $ LogEntry "Starting" [("config", repr config)]
        case filter somethingToDo (moveConfig config) of
          [] -> do
              logWarning "nothing to do, exiting"
              liftBase exitSuccess
          mcs -> watchManyClosedFilesRecursively $
                     map (dirToWatch &&& inotifyHandler) mcs
  where
    logErrorHandler (e :: SomeException) = do
        logError $ LogEntry "unhandledError"
                            [("error", Text.pack (displayException e))]
        liftIO exitFailure
    somethingToDo (MoveConfig _ Nothing Nothing) = False
    somethingToDo _                              = True
    runLog logLevel a = withFDHandler defaultBatchingOptions stderr 0.4 80 $
        \logHandler -> runLoggingT a
            (discardUpToHandler logLevel (logHandler . renderLogEntry))

inotifyHandler
  :: (MonadBaseControl IO io, MonadThrow io, MonadCatch io, MonadState KafkaState io, MonadLog (WithSeverity LogEntry) io, MonadIO io)
  => MoveConfig -> FilePath -> io ()
inotifyHandler config file = when (Path.filename file == "sha256sum.txt")
                           . flip catch handler $ do
    logDebug $ LogEntry "closeEvent" [("file", format fp file)]
    checkDir sha256sumChecker (Path.directory file)
    filesInDir <- ls watchDir

    for_ mMoveTo $ \moveTo -> do
        let moveDir = moveTo </> relDir
        logInfo $ LogEntry "Moving" [ ("dir", format fp watchDir)
                                    , ("moveTo", format fp moveDir)]
        moveDirectory watchDir moveDir
    for_ mTopic $ \topic -> do
        let messages = map Message filesInDir
        logInfo $ LogEntry "AnnouncingFilesInDir" [ ("dir", format fp relDir)
                                                  , ("topic", repr topic)]
        announce topic messages
  where
    MoveConfig _ mMoveTo mTopic = config
    handler (CommandFailedWith cmd args cwd exitCode) =
        logWarning $ LogEntry "checksumInvalid"
                              [ ("cmd", format (unw w) (cmd:args))
                              , ("cwd", repr cwd)
                              , ("exitCode", repr exitCode)]
    watchDir = dirToWatch config </> relDir
    relDir = Path.dirname file

ls :: MonadIO m => FilePath -> m [Text]
ls d = liftIO $ map Text.pack . filter f <$> getDirectoryContents (encodeString d)
  where
    f n = n /= "." && n /= ".." && n /= "sha256sum.txt"
