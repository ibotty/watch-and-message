module System.Watcher.OptionParser where

import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.String (fromString)
import Options.Applicative
import Network.Kafka (KafkaState, mkKafkaState)
import Network.Kafka.Protocol (Host, Port, TopicName)

import System.Watcher.Types

import qualified Control.Monad.Log as Log
import qualified Turtle

parseConfig :: Parser Config
parseConfig = Config <$> parseMoveConfig
                     <*> parseKafkaConfig
                     <*> parseSeverity

parseSeverity :: Parser Log.Severity
parseSeverity = fromMaybe defaultSeverity . listToMaybe . catMaybes <$>
    traverse optional [parseDebug, parseVerbose, parseQuiet]
  where
    parseDebug = flag' Log.Debug
                      (  long "debug"
                      <> short 'd'
                      <> help "Run in debug mode"
                      )
    parseVerbose = boundedOp (+) <$>
                       some (flag' () (  long "verbose"
                                      <> short 'v'
                                      <> help "Increase verbosity"
                                      ))
    parseQuiet  = boundedOp (-) <$>
                      some (flag' () (  long "quiet"
                                     <> short 'q'
                                     <> help "Decrease verbosity"
                                     ))
    boundedOp :: (Int -> Int -> Int) -> [a] -> Log.Severity
    boundedOp op = toEnum . max' Log.Alert . min' Log.Debug
               . op (fromEnum defaultSeverity) . length
    max' :: (Bounded a, Enum a) => a -> Int -> Int
    max' a = max (fromEnum a)
    min' :: (Bounded a, Enum a) => a -> Int -> Int
    min' a = min (fromEnum a)

parseMoveConfig :: Parser [MoveConfig]
parseMoveConfig = some $ MoveConfig
  <$> parseADir "watch" 'w' "directory to watch"
  <*> optional (parseADir "move" 'm' "directory to move files to")
  <*> optional parseTopic

-- parseMoveConfig :: Parser [MoveConfig]
-- parseMoveConfig = fmap (map (uncurry3 MoveConfig)) $ zip3
--   <$> some (parseADir "watch" 'w' "directory to watch")
--   <*> some (parseADir "move" 'm' "directory to move files to")
--   <*> some parseTopic
--   where
--     uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
--     uncurry3 f (a,b,c) = f a b c


parseADir :: String -> Char -> String -> Parser Turtle.FilePath
parseADir optName optShort helptext = fromString <$>
    strOption (  long optName
              <> short optShort
              <> metavar "DIRECTORY"
              <> help helptext
              )

parseTopic :: Parser TopicName
parseTopic = fromString <$>
    strOption (  long "topic"
              <> short 't'
              <> metavar "TOPIC"
              <> help "topic to announce the file"
              )

parseKafkaConfig :: Parser KafkaState
parseKafkaConfig = mkKafkaState
  <$> (fromString <$>
          strOption (  long "application_name"
                    <> help "Kafka application name"
                    <> value "watch-and-message"
                    ))
  <*> parseKafkaAddress

parseKafkaAddress :: Parser (Host, Port)
parseKafkaAddress = (,)
  <$> (fromString <$>
          strOption (  long "host"
                    <> short 'h'
                    <> metavar "KAFKAHOST"
                    <> help "kafka host to connect to"
                    <> value "localhost"
                    ))
  <*> (fromInteger <$>
          option auto (  long "port"
                      <> short 'h'
                      <> metavar "KAFKAHOST"
                      <> help "kafka host to connect to"
                      <> value 9092
                      ))

