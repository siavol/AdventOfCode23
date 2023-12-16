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
      levelTotalLoad (tiltCycle input) `shouldBe` 87

  describe "tiltNCycles" $ do
    it "should roll rocks 2 cycles, load 69" $ do
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
      levelTotalLoad (tiltNCycles 2 input) `shouldBe` 69

    it "should roll rocks 3 cycles, load 69" $ do
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
      levelTotalLoad (tiltNCycles 3 input) `shouldBe` 69

    it "should roll rocks 4 cycles, load 69" $ do
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
      tiltNCycles 4 input `shouldBe` [ ".....#...."
                                     , "....#...O#"
                                     , ".....##..."
                                     , "..O#......"
                                     , ".....OOO#."
                                     , ".O#...O#.#"
                                     , "....O#...O"
                                     , ".......OOO"
                                     , "#...O###.O"
                                     , "#..OO#..OO"]
      levelTotalLoad (tiltNCycles 4 input) `shouldBe` 69

    it "should roll rocks 5 cycles, load 65" $ do
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
      tiltNCycles 5 input `shouldBe` [ ".....#...."
                                     , "....#...O#"
                                     , ".....##..."
                                     , "...#......"
                                     , ".....OOO#."
                                     , ".O#...O#.#"
                                     , "....O#...O"
                                     , "......OOOO"
                                     , "#...O###.O"
                                     , "#..OO#..OO"]
      levelTotalLoad (tiltNCycles 5 input) `shouldBe` 65

  describe "levelTotalLoadAfterTiltNorh" $ do
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
      levelTotalLoadAfterTiltNorh input `shouldBe` 136

  describe "levelTotalLoadAfterNCycles" $ do
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
    it "total load on test level after 1 cycles is 87" $ do
      levelTotalLoadAfterNCycles 1 input `shouldBe` 87
    it "total load on test level after 2 cycles is 69" $ do
      levelTotalLoadAfterNCycles 2 input `shouldBe` 69
    it "total load on test level after 3 cycles is 69" $ do
      levelTotalLoadAfterNCycles 3 input `shouldBe` 69
    it "total load on test level after 4 cycles is 69" $ do
      levelTotalLoadAfterNCycles 4 input `shouldBe` 69
    it "total load on test level after 5 cycles is 69" $ do
      levelTotalLoadAfterNCycles 5 input `shouldBe` 65
    it "total load on test level after 6 cycles is 64" $ do
      levelTotalLoadAfterNCycles 6 input `shouldBe` 64
    it "total load on test level after 7 cycles is 65" $ do
      levelTotalLoadAfterNCycles 7 input `shouldBe` 65
    it "total load on test level after 8 cycles is 63" $ do
      levelTotalLoadAfterNCycles 8 input `shouldBe` 63
    it "total load on test level after 9 cycles is 68" $ do
      levelTotalLoadAfterNCycles 9 input `shouldBe` 68
    it "total load on test level after 10 cycles is 69" $ do
      levelTotalLoadAfterNCycles 10 input `shouldBe` 69
    it "total load on test level after 11 cycles is 69" $ do
      levelTotalLoadAfterNCycles 11 input `shouldBe` 69

    it "should calculate total load on test level after 1000000000 cycles" $ do
      levelTotalLoadAfterNCycles 1000000000 input `shouldBe` 64
