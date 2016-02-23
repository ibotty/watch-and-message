{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Watcher.Log
  ( module Control.Monad.Log
  , renderLogEntry
  , RenderLogEntry(..)
#if ! MIN_VERSION_logging_effect(1,1,0)
  , logAlert, logCritical, logError, logWarning, logNotice, logInfo, logDebug
  , mapLogMessageM
#endif
  , discardUpToHandler
  , unw
  , pair
  ) where

import System.Watcher.Types

import Control.Monad.Log hiding (discardUpToHandler)
import Text.PrettyPrint.Leijen.Text (Doc, text)
import Data.Text (Text)
import Data.Text.Lazy (fromStrict)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Turtle (Format, makeFormat, format, s, w, (%))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Control.Monad (when)
#if ! MIN_VERSION_logging_effect(1,1,0)
import Prelude hiding (log, error)

logAlert, logCritical, logError, logWarning, logNotice, logInfo, logDebug
  :: MonadLog (WithSeverity a) m => a -> m ()
logAlert = logMessage . WithSeverity Alert
logCritical = logMessage . WithSeverity Critical
logError = logMessage . WithSeverity Error
logWarning = logMessage . WithSeverity Warning
logNotice = logMessage . WithSeverity Notice
logInfo = logMessage . WithSeverity Informational
logDebug = logMessage . WithSeverity Debug

mapLogMessageM
  :: MonadLog message' m
  => (message -> m message') -> LoggingT message m a -> m a
mapLogMessageM f m = runLoggingT m ((>>= logMessage) . f)

#endif

class HasSeverity msg where
    severity :: msg -> Severity

instance HasSeverity (WithSeverity msg) where
    severity (WithSeverity svt _) = svt

instance HasSeverity msg => HasSeverity (WithTimestamp msg) where
    severity (WithTimestamp m _ ) = severity m

discardUpToHandler
  :: (Applicative m, HasSeverity msg)
  => Severity -> Handler m msg -> Handler m msg
discardUpToHandler sev handler m =
    when (sev >= severity m) $ handler m

class RenderLogEntry a where
    render :: a -> [Text]

instance RenderLogEntry LogEntry where
    render (LogEntry msg' additional) =
        map (format (pair s)) $ ("msg", msg'):HashMap.toList additional

instance RenderLogEntry a => RenderLogEntry (WithSeverity a) where
    render (WithSeverity sev a) =
        format (pair w) ("sev", show sev) : render a

instance RenderLogEntry a => RenderLogEntry (WithTimestamp a) where
    render (WithTimestamp a ts) =
        format (pair s) ("time", formatTime' ts) : render a
      where
        formatTime' = Text.pack
                    . formatTime defaultTimeLocale
                                 (iso8601DateFormat (Just "%H:%M:%S"))

renderLogEntry :: RenderLogEntry a => a -> Doc
renderLogEntry = text . fromStrict . Text.intercalate " " . render

unw :: Format Text (b -> Text) -> Format r ([b] -> r)
unw f = makeFormat $ Text.unwords . map (format f)

pair :: Format Text (b -> Text) -> Format r ((Text, b) -> r)
pair f = makeFormat $ uncurry (format (s%"=\""%f%"\""))
