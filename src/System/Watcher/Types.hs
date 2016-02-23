{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Watcher.Types
  where

import Prelude hiding (FilePath)
import Data.Aeson (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.HashMap.Strict (HashMap)
import Data.String (IsString, fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Kafka (KafkaState)
import Network.Kafka.Protocol (TopicName)
import Turtle (FilePath, format, fp, w)

import qualified Control.Monad.Log as Log

data MoveConfig = MoveConfig { dirToWatch  :: FilePath
                             , dirToMoveTo :: Maybe Turtle.FilePath
                             , kafkaTopic  :: Maybe TopicName
                             }
  deriving (Show, Eq, Ord)

data Config = Config { moveConfig :: [MoveConfig]
                     , kafkaConfig :: KafkaState
                     , loglevel :: Log.Severity
                     }
  deriving (Show)

data LogEntry = LogEntry { msg :: Text
                         , additionalFields :: HashMap Text Text
                         }
  deriving (Show, Eq)

instance IsString LogEntry where
    fromString s = LogEntry (fromString s) []

data Message = Message { filename :: Text
                       }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Message

renderWhatToDo :: MoveConfig -> LogEntry
renderWhatToDo (MoveConfig watchDir Nothing Nothing) = LogEntry
    "Do nothing"
    [("watchDir", format fp watchDir)]
renderWhatToDo (MoveConfig watchDir (Just moveDir) Nothing) = LogEntry
    "Move from to"
    [ ("watchDir", format fp watchDir)
    , ("moveTo", format fp moveDir)
    ]
renderWhatToDo (MoveConfig watchDir (Just moveDir) (Just topic)) = LogEntry
    "Move from to announce"
    [ ("watchDir", format fp watchDir)
    , ("moveTo", format fp moveDir)
    , ("announceTo", format w topic)
    ]
renderWhatToDo (MoveConfig watchDir Nothing (Just topic)) = LogEntry
    "Announce"
    [ ("watchDir", format fp watchDir)
    , ("announceTo", format w topic)
    ]

defaultSeverity :: Log.Severity
defaultSeverity = Log.Notice
