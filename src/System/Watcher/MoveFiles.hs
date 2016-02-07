module System.Watcher.MoveFiles
  ( moveFiles
  , moveDir
  ) where

import Data.Foldable (traverse_)

import qualified Turtle

moveFiles :: [Turtle.FilePath] -> Turtle.FilePath -> IO ()
moveFiles files to = traverse_ (flip Turtle.mv to) files

moveDir :: Turtle.FilePath -> Turtle.FilePath -> IO ()
moveDir = Turtle.mv
