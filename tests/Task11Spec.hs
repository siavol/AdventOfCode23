module Task11Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task11

spec :: Spec
spec = do
  describe "readSpace" $ do
    it "read star coords from test input" $ do
      let input = [ "...#..."
                  , "......."
                  , "#......"
                  , "......."
                  , "......#"]
      readSpace input `shouldBe` [Coord 4 1, Coord 1 3, Coord 7 5]

  describe "expandX" $ do
    it "expand X coords" $ do
      let input = [Coord 4 1, Coord 1 3, Coord 7 5]
      expandX 1 2 input `shouldBe` [Coord 5 1, Coord 1 3, Coord 8 5]

  describe "expandY" $ do
    it "expand Y coords" $ do
      let input = [Coord 4 1, Coord 1 3, Coord 7 5]
      expandY 1 2 input `shouldBe` [Coord 4 1, Coord 1 4, Coord 7 6]

  describe "expandSpace" $ do
    it "expand space" $ do
      let input = [Coord 1 1, Coord 3 3, Coord 4 5]
      expandSpace 1 input `shouldBe` [Coord 1 1, Coord 4 4, Coord 5 7]

  describe "starsDistanceForSpace" $ do
    let input = [ "...#......"
                , ".......#.."
                , "#........."
                , ".........."
                , "......#..."
                , ".#........"
                , ".........#"
                , ".........."
                , ".......#.."
                , "#...#....."]
    it "calculate stars distance sum for test input with expansion 2" $ do
      starsDistanceForSpace input 1 `shouldBe` 374
    it "calculate stars distance sum for test input with expansion 10" $ do
      starsDistanceForSpace input 9 `shouldBe` 1030
    it "calculate stars distance sum for test input with expansion 100" $ do
      starsDistanceForSpace input 99 `shouldBe` 8410
