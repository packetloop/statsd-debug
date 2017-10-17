import           Control.Exception     (evaluate)
import qualified Data.Map              as Map
import           Network.Statsd
import           Network.Statsd.Parser
import           Test.Hspec
import           Text.ParserCombinators.Parsec


genericParse parser input = case parse parser "(unknown)" input of
  Left _         -> error $ "failed to parse " ++ input
  Right result -> result

metricParse = genericParse parseMetric

eventParse = genericParse parseEvent


type TagList = [(String, String)]

mockEvent :: Maybe EventAggregationKey -> Maybe EventSourceType -> TagList -> Event
mockEvent aggKey sourceType tags = Event
  "apoliceMan" "garbageMan" (EventTimeStamp "1970-01-01T00:00:00Z") (Just $ EventHost "10.2.22.23") aggKey Normal sourceType Info (Map.fromList tags)

mockEventBase :: TagList -> Event
mockEventBase = mockEvent Nothing Nothing

main :: IO ()
main = hspec $
    describe "Network.Statsd.Parser" $ do
        describe "Metrics Parsing" $ do
            it "parses simple metrics" $ do
                metricParse "gas:0.5|g" `shouldBe`  Metric Gauge "gas" 0.5 Map.empty
                metricParse "miles:1|c" `shouldBe` Metric Counter "miles" 1 Map.empty
                metricParse "shift:500|ms" `shouldBe` Metric Timer "shift" 500 Map.empty
            it "parses tags" $
                metricParse "miles:1|c|#gear:4" `shouldBe` Metric Counter "miles" 1 (Map.fromList [("gear", "4")])

        describe "Permutable Parsing" $ do
          it "should permute 1 optional field properly" $
            genericParse permutableEventFields "|h:127.10.10.2" `shouldBe` (defaultTimestamp, Just $ EventHost "127.10.10.2", Nothing, Normal, Nothing, Info)
          it "should permute 2 optional fields properly" $
            genericParse permutableEventFields "|h:127.10.10.2|s:apples" `shouldBe` (defaultTimestamp, Just $ EventHost "127.10.10.2", Nothing, Normal, Just $ EventSourceType "apples", Info)
          it "should give the same result when 2 optional fields are presented in a different order" $
            genericParse permutableEventFields "|s:apples|h:127.10.10.2" `shouldBe` (defaultTimestamp, Just $ EventHost "127.10.10.2", Nothing, Normal, Just $ EventSourceType "apples", Info)
          it "should work with fields which have other default values" $
            genericParse permutableEventFields "|s:apples|p:low|h:127.10.10.2" `shouldBe` (defaultTimestamp, Just $ EventHost "127.10.10.2", Nothing, Low, Just $ EventSourceType "apples", Info)

        describe "Events Parsing" $ do
            it "parseEventLengths should grab numbers" $
                genericParse parseEventLengths "{34,45}" `shouldBe` (34,45)
            it "parsePriority should get dataType" $
                genericParse parsePriority "|p:normal" `shouldBe` Normal
            it "parseHost should get a string of digits" $
                genericParse parseHost "|h:10.2.22.23" `shouldBe` EventHost "10.2.22.23"
            it "parseAlertType should get a string of digits" $
                genericParse parseAlertType "|t:success" `shouldBe` Success
            describe "should parse a complete string into an event" $ do
              describe "when missing a source type name field" $
                it "should parse properly returning Nothing" $
                  eventParse "_e{10,10}:apoliceMan|garbageMan|d:1970-01-01T00:00:00Z|h:10.2.22.23|p:normal|t:info|#abad:day,feeling:day" `shouldBe` mockEventBase [("abad", "day"), ("feeling", "day")]
              describe "when an aggregation key is present" $
                it "should parse properly into the Event" $
                  eventParse "_e{10,10}:apoliceMan|garbageMan|d:1970-01-01T00:00:00Z|h:10.2.22.23|k:poopler|p:normal|t:info|#abad:day,feeling:day" `shouldBe` mockEvent (Just $ EventAggregationKey "poopler") Nothing [("abad", "day"), ("feeling", "day")]
              it "with key:value tags" $
                eventParse "_e{10,10}:apoliceMan|garbageMan|d:1970-01-01T00:00:00Z|h:10.2.22.23|p:normal|s:apple|t:info|#abad:day,feeling:day" `shouldBe` mockEvent Nothing (Just $ EventSourceType "apple") [("abad", "day"), ("feeling", "day")]
              it "with only key tags" $
                eventParse "_e{10,10}:apoliceMan|garbageMan|d:1970-01-01T00:00:00Z|h:10.2.22.23|p:normal|s:apple|t:info|#abad,feeling" `shouldBe` mockEvent Nothing (Just $ EventSourceType "apple") [("abad", ""), ("feeling", "")]
              it "with a mixture of key:value & key tags" $
                eventParse "_e{10,10}:apoliceMan|garbageMan|d:1970-01-01T00:00:00Z|h:10.2.22.23|p:normal|s:apple|t:info|#abad:day,feeling" `shouldBe` mockEvent Nothing (Just $ EventSourceType "apple") [("abad", "day"), ("feeling", "")]
