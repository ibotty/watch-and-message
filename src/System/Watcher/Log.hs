{-# LANGUAGE FlexibleContexts #-}
module System.Watcher.Log
  ( alert, critical, error, warning, notice, info, debug
  , module Control.Monad.Log
  ) where

import Prelude hiding (log, error)
import Control.Monad.Log

alert, critical, error, warning, notice, info, debug
  :: MonadLog (WithSeverity a) m => a -> m ()
alert = logMessage . WithSeverity Alert
critical = logMessage . WithSeverity Critical
error = logMessage . WithSeverity Error
warning = logMessage . WithSeverity Warning
notice = logMessage . WithSeverity Notice
info = logMessage . WithSeverity Informational
debug = logMessage . WithSeverity Debug
