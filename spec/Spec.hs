import           Control.Exception     (evaluate)
import qualified Data.Map              as Map
import           Network.Statsd
import           Network.Statsd.Parser
import           Test.Hspec
-- import           Test.QuickCheck
import           Text.ParserCombinators.Parsec


parseSD input = case parseStats input of
    Left _         -> error $ "failed to parse " ++ input
    Right [stat] -> stat

genericParse parser input = case (parse parser "(unknown)" input) of
  Left _         -> error $ "failed to parse " ++ input
  Right result -> result

metricParse input = genericParse parseMetric input

eventParse input = genericParse parseEvent input



mockEvent1 :: [(String, String)] -> Event
mockEvent1 tags = Event
  -- "apoliceMan" "garbageMan" (Just "2233") (Just "2122") (Just "") (Just "normal") (Just "info") (Just "") (Map.fromList [("abad", "feeling")])
  "apoliceMan" "garbageMan" (Just $ EventTimeStamp "2233") (Just $ EventHost "2122") (Just Normal) (Just Info) (Map.fromList tags)
  -- "apoliceMan" "garbageMan" (Just "2233") (Just "2122") (Just Normal) (Just Info) (Map.fromList [("abad", "feeling")])


main :: IO ()
main = hspec $
    describe "Network.Statsd.Parser" $ do
        describe "Metrics Parsing" $ do
            it "parses simple metrics" $ do
                metricParse "gas:0.5|g" `shouldBe`  Metric Gauge "gas" 0.5 Map.empty
                metricParse "miles:1|c" `shouldBe` Metric Counter "miles" 1 Map.empty
                metricParse "shift:500|ms" `shouldBe` Metric Timer "shift" 500 Map.empty
            it "parses tags" $ do
                metricParse "miles:1|c#gear:4" `shouldBe` Metric Counter "miles" 1 (Map.fromList [("gear", "4")])

        describe "Events Parsing" $ do
            it "parseEventLengths should grab numbers" $ do
                (genericParse parseEventLengths "{34,45}") `shouldBe` (34,45)
            it "parsePriority should get dataType" $ do
                (genericParse parsePriority "|p:normal") `shouldBe` Normal
            it "parseHost should get a string of digits" $ do
                (genericParse parseHost "|h:2122") `shouldBe` (EventHost "2122")
            it "parseAlertType should get a string of digits" $ do
                (genericParse parseAlertType "|t:success") `shouldBe` Success
            describe "should parse a complete string into an event" $ do
              it "with key:value tags" $ do
                eventParse "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad:day,feeling:day" `shouldBe` mockEvent1 [("abad", "day"), ("feeling", "day")]
              it "with only key tags" $ do
                eventParse "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad,feeling" `shouldBe` mockEvent1 [("abad", ""), ("feeling", "")]
              it "with a mixture of key:value & key tags" $ do
                eventParse "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad:day,feeling" `shouldBe` mockEvent1 [("abad", "day"), ("feeling", "")]
