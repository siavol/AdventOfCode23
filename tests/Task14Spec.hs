module Task14Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task14
import Task14 (rollRoundRocksRight)

spec :: Spec
spec = do
  describe "rollRoundRocksRight" $ do
    it "should roll one rock" $ do
      rollRoundRocksRight "O." `shouldBe` ".O"
    it "should roll multiple rocks" $ do
      rollRoundRocksRight "O.O.O" `shouldBe` "..OOO"
    it "should stop rolling at stone" $ do
      rollRoundRocksRight "O.O#O." `shouldBe` ".OO#.O"

  describe "tiltLevelNorth" $ do
    it "should roll rocks on test level" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      tiltLevelNorth input `shouldBe` [ "OOOO.#.O.."
                                      , "OO..#....#"
                                      , "OO..O##..O"
                                      , "O..#.OO..."
                                      , "........#."
                                      , "..#....#.#"
                                      , "..O..#.O.O"
                                      , "..O......."
                                      , "#....###.."
                                      , "#....#...."]
  describe "tiltLevelWest" $ do
    it "should roll rocks on test level" $ do
      let input = [ "OOOO.#.O.."
                  , "OO..#....#"
                  , "OO..O##..O"
                  , "O..#.OO..."
                  , "........#."
                  , "..#....#.#"
                  , "..O..#.O.O"
                  , "..O......."
                  , "#....###.."
                  , "#....#...."]
      tiltLevelWest input `shouldBe` [ "OOOO.#O..."
                                      , "OO..#....#"
                                      , "OOO..##O.."
                                      , "O..#OO...."
                                      , "........#."
                                      , "..#....#.#"
                                      , "O....#OO.."
                                      , "O........."
                                      , "#....###.."
                                      , "#....#...."]

  describe "tiltLevelSouth" $ do
    it "should roll rocks on test level" $ do
      let input = [ "OOOO.#O..."
                  , "OO..#....#"
                  , "OOO..##O.."
                  , "O..#OO...."
                  , "........#."
                  , "..#....#.#"
                  , "O....#OO.."
                  , "O........."
                  , "#....###.."
                  , "#....#...."]
      tiltLevelSouth input `shouldBe` [ ".....#...."
                                      , "....#.O..#"
                                      , "O..O.##..."
                                      , "O.O#......"
                                      , "O.O....O#."
                                      , "O.#..O.#.#"
                                      , "O....#...."
                                      , "OO....OO.."
                                      , "#O...###.."
                                      , "#O..O#...."]

  describe "tiltLevelEast" $ do
    it "should roll rocks on test level" $ do
      let input = [ ".....#...."
                  , "....#.O..#"
                  , "O..O.##..."
                  , "O.O#......"
                  , "O.O....O#."
                  , "O.#..O.#.#"
                  , "O....#...."
                  , "OO....OO.."
                  , "#O...###.."
                  , "#O..O#...."]
      tiltLevelEast input `shouldBe` [ ".....#...."
                                     , "....#...O#"
                                     , "...OO##..."
                                     , ".OO#......"
                                     , ".....OOO#."
                                     , ".O#...O#.#"
                                     , "....O#...."
                                     , "......OOOO"
                                     , "#...O###.."
                                     , "#..OO#...."]

  describe "tiltCycle" $ do
    it "should roll rocks on test level" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      tiltCycle input `shouldBe` [ ".....#...."
                                 , "....#...O#"
                                 , "...OO##..."
                                 , ".OO#......"
                                 , ".....OOO#."
                                 , ".O#...O#.#"
                                 , "....O#...."
                                 , "......OOOO"
                                 , "#...O###.."
                                 , "#..OO#...."]

  describe "tiltNCycles" $ do
    it "should roll rocks 2 cycles" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      tiltNCycles 2 input `shouldBe` [ ".....#...."
                                     , "....#...O#"
                                     , ".....##..."
                                     , "..O#......"
                                     , ".....OOO#."
                                     , ".O#...O#.#"
                                     , "....O#...O"
                                     , ".......OOO"
                                     , "#..OO###.."
                                     , "#.OOO#...O"]

    it "should roll rocks 3 cycles" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      tiltNCycles 3 input `shouldBe` [ ".....#...."
                                     , "....#...O#"
                                     , ".....##..."
                                     , "..O#......"
                                     , ".....OOO#."
                                     , ".O#...O#.#"
                                     , "....O#...O"
                                     , ".......OOO"
                                     , "#...O###.O"
                                     , "#.OOO#...O"]

  describe "levelTotalLoad" $ do
    it "should calculate total load on test level" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      levelTotalLoad input `shouldBe` 136

  describe "levelTotalLoadAfterNCycles" $ do
    xit "should calculate total load on test level after 1000000000 cycles" $ do
      let input = [ "O....#...."
                  , "O.OO#....#"
                  , ".....##..."
                  , "OO.#O....O"
                  , ".O.....O#."
                  , "O.#..O.#.#"
                  , "..O..#O..O"
                  , ".......O.."
                  , "#....###.."
                  , "#OO..#...."]
      levelTotalLoadAfterNCycles 1000000000 input `shouldBe` 64
