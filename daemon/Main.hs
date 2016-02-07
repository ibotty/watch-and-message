{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative (execParser, fullDesc, info, progDesc, helper)
import System.Watcher.Types
import System.Watcher.OptionParser

import qualified Control.Monad.Log as Log
import qualified Turtle as Turtle


opts = info (helper <*> parseConfig)
            (  fullDesc
            <> progDesc "Watch a directory and post ")

main :: IO ()
main = do
    config <- execParser opts
    print config
