module Task5Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Task5

spec :: Spec
spec = do
  describe "Range" $ do
    it "returns Range when reading valid string" $ do
      read "1 2 3" `shouldBe` Range { destStart = 1, srcStart = 2, len = 3 }

  describe "listFromRange" $ do
    it "returns list of tuples from range" $ do
      listFromRange (Range 50 98 2) `shouldBe` [(98, 50), (99, 51)]

  describe "readMap" $ do
    it "returns map from list of strings" $ do
      readMap ["50 98 2", "52 50 3"] `shouldBe` [ Range { destStart = 50, srcStart = 98, len = 2 }
                                                , Range { destStart = 52, srcStart = 50, len = 3 }]

  describe "fromSource" $ do
    it "returns value from map" $ do
      fromSource [Range {destStart=2, srcStart=1, len=2}] 1 `shouldBe` 2

    it "returns key when value is not in map" $ do
      fromSource [Range {destStart=2, srcStart=1, len=2}] 4 `shouldBe` 4

  describe "readPipeline" $ do
    it "returns list of maps from list of strings" $ do
      readPipeline [ "seed-to-soil map:"
                   , "50 98 2"
                   , "52 50 3"
                   , ""
                   , "soil-to-fertilizer map:"
                   , "1 2 3"
                   , "10 5 1"] `shouldBe` [ [ Range { destStart = 50, srcStart = 98, len = 2 }
                                            , Range { destStart = 52, srcStart = 50, len = 3 } ]
                                          , [ Range { destStart = 1, srcStart = 2, len = 3 }
                                            , Range { destStart = 10, srcStart = 5, len = 1 } ] ]

  describe "pipeline" $ do
    let ppln = [ [ Range {srcStart=1, destStart=3, len=3}
                 , Range {srcStart=4, destStart=10, len=2} ]
               , [ Range {srcStart=9, destStart=20, len=3} ] ]
    it "returns 22 from pipeline when source 5" $ do
      pipeline ppln 5 `shouldBe` 22
    it "returns 3 from pipeline when source 1" $ do
      pipeline ppln 1 `shouldBe` 3

  describe "lowestLocationSeedBySeed" $ do
    let input = [ "seeds: 79 14 55 13"
                , ""
                , "seed-to-soil map:"
                , "50 98 2"
                , "52 50 48"
                , ""
                , "soil-to-fertilizer map:"
                , "0 15 37"
                , "37 52 2"
                , "39 0 15"
                , ""
                , "fertilizer-to-water map:"
                , "49 53 8"
                , "0 11 42"
                , "42 0 7"
                , "57 7 4"
                , ""
                , "water-to-light map:"
                , "88 18 7"
                , "18 25 70"
                , ""
                , "light-to-temperature map:"
                , "45 77 23"
                , "81 45 19"
                , "68 64 13"
                , ""
                , "temperature-to-humidity map:"
                , "0 69 1"
                , "1 0 69"
                , ""
                , "humidity-to-location map:"
                , "60 56 37"
                , "56 93 4"]
    it "returns 35 from test input" $ do
      lowestLocationSeedBySeed input `shouldBe` 35

  describe "pairs" $ do
    it "returns list of pairs from list" $ do
      pairs [1, 2, 3, 4] `shouldBe` [(1, 2), (3, 4)]

  describe "listFromPairs" $ do
    it "returns list from list of pairs" $ do
      listFromPairs [(1, 2), (5, 3)] `shouldBe` [1, 2, 5, 6, 7]

  describe "seedRange" $ do
    it "returns list of seeds from list of pairs" $ do
      seedRange [1, 2, 5, 3] `shouldBe` [1, 2, 5, 6, 7]

  describe "lowestLocationBySeedRange" $ do
    let input = [ "seeds: 79 14 55 13"
                , ""
                , "seed-to-soil map:"
                , "50 98 2"
                , "52 50 48"
                , ""
                , "soil-to-fertilizer map:"
                , "0 15 37"
                , "37 52 2"
                , "39 0 15"
                , ""
                , "fertilizer-to-water map:"
                , "49 53 8"
                , "0 11 42"
                , "42 0 7"
                , "57 7 4"
                , ""
                , "water-to-light map:"
                , "88 18 7"
                , "18 25 70"
                , ""
                , "light-to-temperature map:"
                , "45 77 23"
                , "81 45 19"
                , "68 64 13"
                , ""
                , "temperature-to-humidity map:"
                , "0 69 1"
                , "1 0 69"
                , ""
                , "humidity-to-location map:"
                , "60 56 37"
                , "56 93 4"]
    it "returns 46 from test input" $ do
      lowestLocationBySeedRange input `shouldBe` 46
