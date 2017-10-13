import           Control.Exception     (evaluate)
import qualified Data.Map              as Map
import           Network.Statsd
import           Network.Statsd.Parser
import           Test.Hspec
-- import           Test.QuickCheck
import           Text.ParserCombinators.Parsec


parseM input = case parseMetrics input of
    Left _         -> error $ "failed to parse " ++ input
    Right [metric] -> metric

parseE input = case parseEvents input of
    Left _         -> error $ "failed to parse " ++ input
    Right [event] -> event

parseSD input = case parseStats input of
    Left _         -> error $ "failed to parse " ++ input
    Right [stat] -> stat


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
                parseM "gas:0.5|g" `shouldBe` Metric Gauge "gas" 0.5 Map.empty
                parseM "miles:1|c" `shouldBe` Metric Counter "miles" 1 Map.empty
                parseM "shift:500|ms" `shouldBe` Metric Timer "shift" 500 Map.empty
            it "parses tags" $ do
                parseM "miles:1|c#gear:4" `shouldBe` Metric Counter "miles" 1 (Map.fromList [("gear", "4")])

        describe "Events Parsing" $ do
            it "parseEventLengths should grab numbers" $ do
                (parse parseEventLengths "(unknown)" "{34,45}") `shouldBe` Right (34,45)
            it "parsePriority should get dataType" $ do
                (parse parsePriority "(unknown)" "p:normal") `shouldBe` Right Normal
            it "parseHost should get a string of digits" $ do
                (parse parseHost "(unknown)" "h:2122") `shouldBe` Right (EventHost "2122")
            it "parseAlertType should get a string of digits" $ do
                (parse parseAlertType "(unknown)" "t:success") `shouldBe` Right Success
            -- it "basically parses stuff" $ do
            --     parseEvents "_e{34,45}" `shouldBe` Right (34,45)
            describe "should parse a complete string into an event" $ do
              it "with key:value tags" $ do
              -- _e{title.length,text.length}:title|text|d:timestamp|h:hostname|p:priority|t:alert_type|#tag1,tag2
                parseEvents "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad:day,feeling:day" `shouldBe` Right [mockEvent1 [("abad", "day"), ("feeling", "day")]]
              it "with only key tags" $ do
              -- _e{title.length,text.length}:title|text|d:timestamp|h:hostname|p:priority|t:alert_type|#tag1,tag2
                parseEvents "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad,feeling" `shouldBe` Right [mockEvent1 [("abad", ""), ("feeling", "")]]
              it "with a mixture of key:value & key tags" $ do
              -- _e{title.length,text.length}:title|text|d:timestamp|h:hostname|p:priority|t:alert_type|#tag1,tag2
                parseEvents "_e{34,45}:apoliceMan|garbageMan|d:2233|h:2122|p:normal|t:info|#abad:day,feeling" `shouldBe` Right [mockEvent1 [("abad", "day"), ("feeling", "")]]

        -- describe "text Parsing" $ do
        --     it "should parse stuff" $ do
        --         parse (parseTextSep 8 "eight le|") `shouldBe` "eight le"
