{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Prelude hiding (FilePath)
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
import System.Exit (exitSuccess)
import System.Directory (getDirectoryContents)
import System.IO (stderr)
import System.Watcher
import Turtle (FilePath, repr, format, fp, w)

import qualified Data.Text as Text
import qualified Filesystem.Path as Path


opts :: ParserInfo Config
opts = info (helper <*> parseConfig)
            (  fullDesc
            <> progDesc "Watch a directory and post ")

main :: IO ()
main = do
    config <- execParser opts
    runLog (loglevel config) . flip evalStateT (kafkaConfig config) $ do
        traverse_ (logInfo . renderWhatToDo) $ moveConfig config
        logDebug $ LogEntry "Starting" [("config", repr config)]
        case moveConfig config of
          [] -> do
              logWarning "nothing to do, exiting"
              liftBase exitSuccess
          mcs -> watchWriteClosedFilesInThread mcs inotifyHandler
  where
    runLog logLevel a = withFDHandler defaultBatchingOptions stderr 0.4 80 $
        \logHandler -> runLoggingT a
            (discardUpToHandler logLevel (logHandler . renderLogEntry))

inotifyHandler
  :: (MonadBaseControl IO io, MonadThrow io, MonadCatch io, MonadState KafkaState io, MonadLog (WithSeverity LogEntry) io, MonadIO io)
  => MoveConfig -> FilePath -> io ()
inotifyHandler config file = when (Path.filename file == "sha256sum.txt")
                           . flip catch handler $ do
    logDebug $ LogEntry "closeEvent" [("file", format fp file)]
    checkDir sha256sumChecker (dirToWatch config)
    let MoveConfig _ mMoveTo mTopic = config
    for_ mMoveTo $ \moveTo -> do
        logInfo $ LogEntry "Moving" [ ("dir", format fp dir)
                                    , ("moveTo", format fp moveTo)]
        moveDir dir moveTo
    for_ mTopic $ \topic -> do
        logInfo $ LogEntry "AnnouncingFilesInDir" [ ("dir", format fp dir)
                                        , ("topic", repr topic)]
        messages <- map mkMessage <$> ls dir
        announce topic messages
  where
    handler (CommandFailedWith cmd args cwd exitCode) =
        logWarning $ LogEntry "checksumInvalid"
                              [ ("cmd", format (unw w) (cmd:args))
                              , ("cwd", repr cwd)
                              , ("exitCode", repr exitCode)]
    dir = dirToWatch config
    mkMessage = Message

ls :: MonadIO m => FilePath -> m [Text]
ls d = liftIO $ map Text.pack . filter f <$> getDirectoryContents (show d)
  where
    f n = n /= "." && n /= ".." && n /= "sha256sum.txt"
