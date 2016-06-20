{-# LANGUAGE FlexibleContexts #-}
module System.Watcher.Announce
  ( announce
  , mkKafkaState
  , KafkaState
  , KafkaClientError(..)
  , TopicName
  ) where

import Control.Monad (void)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Network.Kafka
import Network.Kafka.Producer
import Network.Kafka.Protocol hiding (Message)
import System.Watcher.Types

import qualified Data.Aeson as Aeson
import qualified Network.Kafka.Protocol as Kafka

runKafka' :: (MonadThrow m, MonadState KafkaState m, MonadIO m) => ExceptT KafkaClientError m a -> m a
runKafka' m = runExceptT m >>= either throwM return

rawAnnounce
  :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, MonadState KafkaState m)
  => TopicName -> [Kafka.Message] -> m ()
rawAnnounce topic = runKafka' . void . produceMessages . map (TopicAndMessage topic)

announce
  :: (MonadBaseControl IO m, MonadIO m, MonadThrow m, MonadState KafkaState m)
  => TopicName -> [Message] -> m ()
announce topic = rawAnnounce topic . map makeKeyedMessage'
  where
    makeKeyedMessage' m = makeKeyedMessage (key m) (rawMessage m)
    key = encodeUtf8 . filename
    rawMessage = toStrict . Aeson.encode
