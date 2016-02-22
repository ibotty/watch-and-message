{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module System.Watcher.CheckFiles
  ( checkDir
  , parseShellCmd
  , sha256sumChecker
  , CommandFailedWith(..)
  , unw
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (Exception, MonadThrow, displayException, throwM)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Filesystem.Path.CurrentOS (encodeString)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (createProcess, cwd, proc, waitForProcess)
-- import System.Directory (getDirectoryContents)
import Turtle ((</>))

import qualified Data.Text as Text
import qualified Turtle

data CommandFailedWith = CommandFailedWith FilePath [String] FilePath ExitCode
  deriving (Show, Eq)
instance Exception CommandFailedWith where
    displayException (CommandFailedWith cmd args cwd' exitcode) =
      "command '" ++ unwords (cmd:args) ++ "' in " ++ cwd' ++ " failed with " ++ show exitcode

-- data CannotParseShellCmd = CommandEmpty
--                          | CommandDoesNotExist Text
--   deriving (Show, Eq)
-- instance Exception CannotParseShellCmd where
--     displayException CommandEmpty = "Cannot parse shell command: empty string"
--     displayException (CommandDoesNotExist cmd) =
--       "Cannot parse shell command: command does not exist: " ++ show cmd

data FilesChecker m =
    DirChecker (Turtle.FilePath -> m ())
--  | FilesChecker ([Turtle.FilePath] -> m ())
  | ManyChecks (Turtle.FilePath -> m [FilesChecker m])

-- | checkDir runs the DirChecker and throws a 'Turtle.ProcFailed' for
-- non-zero exit codes.
checkDir :: MonadIO io => FilesChecker io -> Turtle.FilePath -> io ()
checkDir (DirChecker cmd) dir = cmd dir
--checkDir (FilesChecker cmd) dir = ls dir >>= cmd
--    flip Turtle.shells (return mempty) $ cmd [dir </> "/"]
checkDir (ManyChecks mkChecker) file =
    traverse_ (flip checkDir file) =<< mkChecker file

-- checkerWithFilesAppended :: (MonadIO m, MonadThrow m) => Text -> m FilesChecker
-- checkerWithFilesAppended (parseShellCmd -> (cmd:args)) =
--     DirChecker (\files -> (cmd, args ++ map (Text.pack . show) files))
-- checkerWithFilesAppended (parseShellCmd -> _) = throwM CommandEmpty

-- checksumChecker :: MonadThrow m
--                 => [Text]          -- ^ command with arguments
--                 -> Turtle.FilePath -- ^ basename of check file
--                 -> m FilesChecker
-- checksumChecker (cmd:args) toAppend = pure . ManyChecks $
--     \files -> map checkerInDir (dirs files)
--   where
--     checkerInDir dir = DirChecker $ \dir
--         const (cmd, args ++ [format fp $ dir </> toAppend])
--     dirs = Set.toList . Set.map Turtle.directory . Set.fromList
-- checksumChecker [] _ = throwM CommandEmpty

parseShellCmd :: Text -> [Text]
parseShellCmd = Text.words

-- TODO: specify CWD
sha256sumChecker :: (MonadThrow m, MonadIO m) => FilesChecker m
sha256sumChecker = DirChecker $ \dir ->
    liftIO (createAndWait dir) >>= \case
      ExitSuccess -> return ()
      e           -> throwM (exc dir e)
  where
    exc dir = CommandFailedWith "sha256sum" (args dir) (show dir)
    createAndWait dir = do
        (_, _, _, handle) <- createProcess (procSpec dir)
        waitForProcess handle
    procSpec dir = (proc "sha256sum" (args dir)) { cwd = Just (encodeString dir) }
    args :: Turtle.FilePath -> [String]
    args dir = ["--strict", "--quiet", "-c"
               , encodeString (dir </> "sha256sum.txt")]

unw :: Turtle.Format Text (b -> Text) -> Turtle.Format r ([b] -> r)
unw f = Turtle.makeFormat $ Text.unwords . map (Turtle.format f)
