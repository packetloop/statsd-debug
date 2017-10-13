module Network.Statsd where

import           Data.List (intercalate)
import qualified Data.Map  as Map

type Name = String
type Value = Double
type Tags = Map.Map String String

data StatsD = MetricD Metric | EventD Event
data MetricType = Gauge | Counter | Timer | Histogram deriving (Show, Eq)
data Metric = Metric MetricType Name Value Tags deriving Eq

type EventTitle = String
type EventText = String
type EventTimeStamp = String
type EventHost = String
type EventAggregationKey = String
data EventPriority = Normal | Low
    deriving (Eq, Show)
type EventSourceType = String
data EventAlert = Error | Warning | Info | Success
    deriving (Eq, Show)

-- data Event = Event EventTitle EventText (Maybe EventTimeStamp) (Maybe EventHost)
data Event = Event {
    eventTitle     :: EventTitle,
    eventText      :: EventText,
    eventTimeStamp :: (Maybe EventTimeStamp),
    eventHost      :: Maybe EventHost,
    aggregationKey :: Maybe EventAggregationKey,
    priority       :: Maybe String,
    -- priority       :: Maybe EventPriority,
    sourceTypeName :: Maybe EventSourceType,
    alertType      :: Maybe String,
    -- alertType      :: Maybe EventAlert,
    eventTags      :: Tags
} deriving (Eq, Show)

instance Show Metric where
    show (Metric metricType metricName metricValue metricTags) =
        intercalate " " [formatType metricType, formatName metricName, formatValue metricValue, formatTags metricTags]

formatType = show
formatName name = "`" ++ name ++ "'"
formatValue = show

formatTags tags =
    let
        formatTag (name, value) = name ++ ":" ++ value
        formattedTags = map formatTag $ Map.toList tags
    in
        intercalate ", " formattedTags
