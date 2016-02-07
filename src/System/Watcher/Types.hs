module System.Watcher.Types
  where

import Data.ByteString (ByteString)
import Network.Kafka (KafkaState)

import qualified Control.Monad.Log as Log
import qualified Turtle

data MoveConfig = MoveConfig { dirToWatch  :: Turtle.FilePath
                             , dirToMoveTo :: Turtle.FilePath
                             , kafkaTopic  :: ByteString
                             }
  deriving (Show, Eq, Ord)

data Config = Config { moveConfig :: [MoveConfig]
                     , kafkaConfig :: KafkaState
                     , severity :: Log.Severity
                     }
  deriving (Show)

defaultSeverity :: Log.Severity
defaultSeverity = Log.Notice
