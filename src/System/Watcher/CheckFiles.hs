{-# LANGUAGE ViewPatterns #-}
module System.Watcher.CheckFiles
  ( checkFiles
  , checkerWithFilesAppended
  , checksumChecker
  , parseShellCmd
  ) where

import Control.Monad.Catch (Exception, MonadThrow, displayException, throwM)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Filesystem.Path.CurrentOS (toText)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Turtle

data CannotParseShellCmd = CommandEmpty
                         | CommandDoesNotExist Text
  deriving (Show, Eq)
instance Exception CannotParseShellCmd where
    displayException CommandEmpty = "Cannot parse shell command: empty string"
    displayException (CommandDoesNotExist cmd) =
      "Cannot parse shell command: command does not exist: " ++ show cmd

data FilesChecker =
    CheckWithCommand ([Turtle.FilePath] -> (Text, [Text])) -- ^ Get command to run with args
  | CheckWithShell   ([Turtle.FilePath] -> Text)           -- ^ Get command to run with shell
  | ManyChecks ([Turtle.FilePath] -> [FilesChecker])

-- | checkFiles runs the FilesChecker and throws a 'Turtle.ProcFailed' for
-- non-zero exit codes.
checkFiles :: FilesChecker -> [Turtle.FilePath] -> IO ()
checkFiles (CheckWithCommand mkCmd) files =
    flip (uncurry Turtle.procs) (return mempty) $ mkCmd files
checkFiles (CheckWithShell mkCmd) files =
    flip Turtle.shells (return mempty) $ mkCmd files
checkFiles (ManyChecks mkChecker) files =
    traverse_ (flip checkFiles files) (mkChecker files)

checkerWithFilesAppended :: MonadThrow m => Text -> m FilesChecker
checkerWithFilesAppended (parseShellCmd -> (cmd:args)) = pure $
    CheckWithCommand (\files -> (cmd, args ++ map (Text.pack . show) files))
checkerWithFilesAppended (parseShellCmd -> _) = throwM CommandEmpty

checksumChecker :: MonadThrow m
                => [Text]          -- ^ command with arguments
                -> Turtle.FilePath -- ^ basename of check file
                -> m FilesChecker
checksumChecker (cmd:args) basename = pure . ManyChecks $
    \files -> map checkerInDir (dirs files)
  where
    checkerInDir dir = CheckWithCommand $
        const (cmd, args ++ [toText' $ dir Turtle.</> basename])
    dirs = Set.toList . Set.map Turtle.directory . Set.fromList
    toText' = either (error . ("Cannot decode file: " ++) . show) id . toText
checksumChecker [] _ = throwM CommandEmpty

parseShellCmd :: Text -> [Text]
parseShellCmd = Text.words
