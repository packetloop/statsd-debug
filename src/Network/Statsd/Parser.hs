{-# LANGUAGE FlexibleContexts #-}

module Network.Statsd.Parser where

import qualified Data.Map                      as Map
import           Network.Statsd
import           Text.Parsec.Numbers
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Perm

parseStats :: String -> Either ParseError [StatsD]
parseStats input = parse parseString "(unknown)" input

parseString :: GenParser Char st [StatsD]
parseString = do
    result <- many parseLine
    eof
    return result

parseLine = do
    result <- (MetricD <$> parseMetric) <|>(EventD <$> parseEvent)
    optional $ char '\n'
    return result

parseMetric = do
    name <- parseName
    char ':'
    value <- parseValue
    metricType <- parseType
    tags <- option Map.empty parseTags
    return $ Metric metricType name value tags

parseEvent = do
    string "_e"
    (a, b) <- parseEventLengths
    char ':'
    parseEventRest (a,b)

parseEventLengths = do
  braces <- between (char '{') (char '}') parseNumTuple
  return braces

parseNumTuple = do
  titleLength <- parseIntegral
  char ','
  textLength <- parseIntegral
  return (titleLength, textLength)

parseEventText n = count n anyChar

parseEventRest (a, b) = do
    title <- parseEventText a
    char '|'
    text <- parseEventText b
    (timestamp, host, aggKey, priority, sourceType, alertType) <- permutableEventFields
    tags <- option Map.empty parseTags
    return Event {
      eventTitle = title,
      eventText = text,
      eventTimeStamp = timestamp,
      eventHost = host,
      aggregationKey = aggKey,
      priority = priority,
      sourceTypeName = sourceType,
      alertType = alertType,
      eventTags = tags
    }

-- TODO: This is obviously shit - fix this...should get current time - e.g.
-- defaultTimestamp :: IO EventTimeStamp
-- defaultTimestamp = EventTimeStamp getCurrentTime
defaultTimestamp :: EventTimeStamp
defaultTimestamp = EventTimeStamp "1970-01-01T00:00:00Z"

permutableEventFields = permute (tuple
      <$?> (defaultTimestamp, parseTimestamp)
      <|?> (Nothing, Just <$> parseHost)
      <|?> (Nothing, Just <$> parseAggregationKey)
      <|?> (Normal, parsePriority)
      <|?> (Nothing, Just <$> parseSourceType)
      <|?> (Info, parseAlertType)
    ) where
  tuple a b c d e f = (a, b, c, d, e, f)

-- | Optional Parsers/Fields

-- | Format:
-- 1970-01-01T00:00:00Z
parseTimestamp = do
    try $ string "|d:"
    EventTimeStamp <$> many (digit <|> oneOf ['-', 'T', 'Z', ':'])

parseHost = do
    try $ string "|h:"
    EventHost <$> many (digit <|> char '.')

parseAggregationKey = do
    try $ string "|k:"
    EventAggregationKey <$> (many1 $ noneOf "#|\n")

parsePriority = do
    try $ string "|p:"
    getPriority <$>choice [string "normal", string "low"]

parseSourceType = do
    try $ string "|s:"
    EventSourceType <$> (many1 $ noneOf "#|\n")

parseAlertType = do
    try $ string "|t:"
    getAlert <$> choice [string "error", string "warning", string "info", string "success"]

parseName = do
    name <- many1 $ noneOf ":\n"
    return name

parseValue = do
    value <- parseFloat
    return value

parseType = do
    char '|'
    metricType <- many1 $ noneOf "#|\n"
    return $ mapType metricType

parseTags = do
    string "|#"
    tags <- many1 (try parseTagWithValue <|> parseTagWithoutValue)
    return (Map.fromList tags)

parseTagWithValue = do
  optional $ char ','
  name <- many1 $ noneOf ":,\n"
  val <- parseTagValue
  return (name, val)

parseTagWithoutValue = do
  optional $ char ','
  name <- many1 $ noneOf ":,\n"
  return (name, "")

parseTagValue :: GenParser Char st String
parseTagValue = do
  char ':'
  value <- many $ noneOf ",\n"
  return value

getPriority :: String -> EventPriority
getPriority "normal" = Normal
getPriority "low" = Low

getAlert :: String -> EventAlert
getAlert "error" = Error
getAlert "warning" = Warning
getAlert "info" = Info
getAlert "success" = Success


mapType :: String -> MetricType
mapType "g"  = Gauge
mapType "ms" = Timer
mapType "c"  = Counter
mapType "h"  = Histogram
mapType x    = error $ "invalid type: " ++ x
