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
newtype EventTimeStamp = EventTimeStamp String
  deriving (Eq, Show)
newtype EventHost = EventHost String
  deriving (Eq, Show)
newtype EventAggregationKey = EventAggregationKey String
  deriving (Eq, Show)
data EventPriority = Normal | Low
    deriving (Eq, Show)
newtype EventSourceType = EventSourceType String
  deriving (Eq, Show)
data EventAlert = Error | Warning | Info | Success
    deriving (Eq, Show)

-- data Event = Event EventTitle EventText (Maybe EventTimeStamp) (Maybe EventHost)
data Event = Event {
    eventTitle     :: EventTitle,
    eventText      :: EventText,
    eventTimeStamp :: EventTimeStamp,
    eventHost      :: Maybe EventHost,
    aggregationKey :: Maybe EventAggregationKey,
    priority       :: EventPriority,
    sourceTypeName :: Maybe EventSourceType,
    alertType      :: EventAlert,
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
