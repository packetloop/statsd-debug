{-# LANGUAGE FlexibleContexts #-}

-- module Network.Statsd.Parser (parseMetrics, parseEvents) where
module Network.Statsd.Parser where

import qualified Data.Map                      as Map
import           Network.Statsd
import           Text.Parsec.Numbers
import           Text.ParserCombinators.Parsec

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

parseMetrics :: String -> Either ParseError [Metric]
parseMetrics input = parse parseMetricString "(unknown)" input

parseEvents :: String -> Either ParseError [Event]
-- parseEvents :: String -> Either ParseError (Integer, Integer)
parseEvents input = parse parseEventString "(unknown)" input

-- GenParser Char st [Metric]
-- Creates a Parser that takes a
parseMetricString :: GenParser Char st [Metric]
parseMetricString = do
    result <- many parseMetricLine
    eof
    return result

-- parseEventString :: GenParser Char st (Integer, Integer)
parseEventString :: GenParser Char st [Event]
parseEventString = do
    -- result <- many parseLine
    result <- many parseEventLine
    eof
    return result

parseMetricLine = do
    result <- parseMetric
    optional $ char '\n'
    return result

parseEventLine = do
    result <- parseEvent
    optional $ char '\n'
    return result

parseMetric = do
    name <- parseName
    char ':'
    value <- parseValue
    char '|'
    metricType <- parseType
    tags <- option Map.empty parseTags
    return $ Metric metricType name value tags

-- parseEvent
parseEvent = do
    string "_e"
    -- braces <- between (symbol "{") (symbol "}") parseEventLengths
    -- return braces
    (a, b) <- parseEventLengths
    char ':'
    parseEventRest (a,b)

-- parseTextSep :: Int -> Parsec [Char] [Char]
-- parseTextSep n = do
--     sepped <- count n (char '|')
--     char '|'
--     return _

parseEventLengths = do
  braces <- between (char '{') (char '}') parseNumTuple
  return braces

parseNumTuple = do
  -- many digit
  titleLength <- parseIntegral
  char ','
  textLength <- parseIntegral
  return (titleLength, textLength)

parseEventText n = count n anyChar

parseEventRest (a, b) = do
    title <- parseEventText 10
    char '|'
    text <- parseEventText 10
    -- title <- count 10 anyChar
    -- char '|'
    -- text <- count 10 anyChar
    char '|'
    timestamp <- parseTimestamp
    char '|'
    host <- parseHost
    char '|'
    priority <- parsePriority
    char '|'
    alertType <- parseAlertType
    char '|'
    tags <- option Map.empty parseBetterTags
    -- tags <- parseTags
    return Event {
      eventTitle = title,
      eventText = text,
      eventTimeStamp = Just timestamp,
      eventHost = Just host,
      -- aggregationKey = Just "",
      priority = Just priority,
      -- sourceTypeName = Just "",
      alertType = Just $ alertType,
      eventTags = tags
    }
    -- return (titleLength, textLength)

-- parseBar = do

parsePriority = do
    string "p:"
    getPriority <$>choice [string "normal", string "low"]

parseAlertType = do
    string "t:"
    getAlert <$> choice [string "error", string "warning", string "info", string "success"]

parseHost = do
    string "h:"
    EventHost <$> count 4 digit

parseTimestamp = do
    string "d:"
    EventTimeStamp <$> count 4 digit

-- parseName :: Parsec [Char] _
-- parseName :: ParsecT [Char] _ Identity [Char]
-- parseName :: Text.Parsec.Prim.ParsecT [Char] u Data.Functor.Identity.Identity [Char]
parseName = do
    name <- many1 $ noneOf ":\n"
    return name

parseValue = do
    value <- parseFloat
    return value

parseType = do
    metricType <- many1 $ noneOf "#|\n"
    optional $ char '|'
    return $ mapType metricType

parseTags = do
    char '#'
    tags <- many1 parseTag
    return (Map.fromList tags)

parseBetterTags = do
    char '#'
    -- tags <- many1 parseTagWithoutValue
    tags <- many1 (try parseTagWithValue <|> parseTagWithoutValue)
    -- tags <- (many1 parseTagWithValue) <|> (many1 parseTagWithoutValue)
    -- tags <- many1 (parseTagWithValue <|> parseTagWithoutValue)
    -- tags <- many1 (parseTagWithoutValue <|> parseTagWithValue)
    return (Map.fromList tags)

parseTag :: GenParser Char st (String, String)
parseTag = do
    optional $ char ','
    name <- many1 $ noneOf ":,\n"
    char ':'
    value <- many $ noneOf ",\n"
    return (name, value)

-- parseTagWithValue :: GenParser Char st (String, String)
parseTagWithValue = do
  optional $ char ','
  name <- many1 $ noneOf ":,\n"
  val <- parseTagValue
  return (name, val)

-- parseTagWithoutValue :: Text.ParserCombinators.Parsec Char st (String, String)
-- parseTagWithoutValue :: GenParser Char st (String, String)
parseTagWithoutValue = do
  optional $ char ','
  name <- many1 $ noneOf ":,\n"
  return (name, "")


parseBetterTag :: GenParser Char st (String, String)
parseBetterTag = do
    optional $ char ','
    name <- many1 $ noneOf ":,\n"
    -- char ':'
    -- value <- many $ noneOf ",\n"
    val <- parseTagValue
    -- case val of
    --   (Left _) -> val
    --   otherwise -> return (name, val)
    return (name, val)

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
