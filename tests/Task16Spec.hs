module Task16Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Prelude hiding (Right, Left)

import Task16
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "readField" $ do
    it "should read field" $ do
      let field = readField [".\\", "/."]
      readField [".\\", "/."] `shouldBe` Map.fromList [ (Coord 0 0, '.'), (Coord 1 0, '\\')
                                                      , (Coord 0 1, '/'), (Coord 1 1, '.') ]

  describe "traceBeamStep" $ do
    it "shoud go same direction through space" $ do
      let field = readField ["..", ".."]
      let stepsMap = Map.empty
      let coord = Coord 0 0
      let direction = Right
      let (newStepsMap, newSteps) = traceBeamStep field stepsMap (coord, direction)
      newStepsMap `shouldBe` Map.fromList [ ((Coord 0 0, Right), True) ]
      newSteps `shouldBe` [ (Coord 1 0, Right) ]
    it "shoud reflect from forward mirror" $ do
      let field = readField ["..", "./"]
      let stepsMap = Map.empty
      let coord = Coord 1 1
      let direction = Right
      let (newStepsMap, newSteps) = traceBeamStep field stepsMap (coord, direction)
      newStepsMap `shouldBe` Map.fromList [ ((Coord 1 1, Right), True) ]
      newSteps `shouldBe` [ (Coord 1 0, Up) ]
    it "shoud reflect from backward mirror" $ do
      let field = readField [".\\", ".."]
      let stepsMap = Map.empty
      let coord = Coord 1 0
      let direction = Up
      let (newStepsMap, newSteps) = traceBeamStep field stepsMap (coord, direction)
      newStepsMap `shouldBe` Map.fromList [ ((Coord 1 0, Up), True) ]
      newSteps `shouldBe` [ (Coord 0 0, Left) ]
    it "shoud split beam on vertical" $ do
      let field = readField ["..", "|."]
      let stepsMap = Map.empty
      let coord = Coord 0 1
      let direction = Left
      let (newStepsMap, newSteps) = traceBeamStep field stepsMap (coord, direction)
      newStepsMap `shouldBe` Map.fromList [ ((Coord 0 1, Left), True) ]
      newSteps `shouldBe` [ (Coord 0 0, Up), (Coord 0 2, Down) ]
    it "shoud split beam on horizontal" $ do
      let field = readField ["..", ".-"]
      let stepsMap = Map.empty
      let coord = Coord 1 1
      let direction = Down
      let (newStepsMap, newSteps) = traceBeamStep field stepsMap (coord, direction)
      newStepsMap `shouldBe` Map.fromList [ ((Coord 1 1, Down), True) ]
      newSteps `shouldBe` [ (Coord 0 1, Left), (Coord 2 1, Right) ]

  describe "traceBeam" $ do
    it "should trace beam on simple map" $ do
      let field = readField ["\\.."
                            , "./-"
                            ,"\\|."]
      let stepsMap = traceBeams field (Coord 0 0) Right
      stepsMap `shouldBe` Map.fromList [ ((Coord 0 0, Right), True)
                                       , ((Coord 0 1, Down), True)
                                       , ((Coord 0 2, Down), True)
                                       , ((Coord 2 1, Right), True)
                                       , ((Coord 1 1, Up), True)
                                       , ((Coord 1 2, Right), True)]
    xit "should trace beam in test input" $ do
      let field = readField [ ".|...\\...." -- 0
                            , "|.-.\\....." -- 1
                            , ".....|-..."  -- 2
                            , "........|."  -- 3
                            , ".........."  -- 4
                            , ".........\\" -- 5
                            , "..../.\\.."  -- 6
                            , ".-.-/..|.."  -- 7
                            , ".|....-|.\\" -- 8
                            , "..//.|...."] -- 9
      let stepsMap = traceBeams field (Coord 0 0) Right
      stepsMap `shouldBe` Map.fromList [ ((Coord 0 0, Right), True)
                                       , ((Coord 1 0, Right), True)
                                       , ((Coord 1 1, Down), True)
                                       , ((Coord 1 2, Down), True)
                                       , ((Coord 1 3, Down), True)
                                       , ((Coord 1 4, Down), True)
                                       , ((Coord 1 5, Down), True)
                                       , ((Coord 1 6, Down), True)
                                       , ((Coord 1 7, Down), True)
                                       , ((Coord 0 7, Left), True)
                                       , ((Coord 2 7, Right), True)
                                       , ((Coord 3 7, Right), True)
                                       , ((Coord 4 7, Right), True)
                                       , ((Coord 4 6, Up), True)]


  describe "calcEnergizedFields" $ do
    let field = readField [ ".|...\\...."
                          , "|.-.\\....."
                          , ".....|-..."
                          , "........|."
                          , ".........."
                          , ".........\\"
                          , "..../.\\\\.."
                          , ".-.-/..|.."
                          , ".|....-|.\\"
                          , "..//.|...."]
    it "should return 46 energized fields for test input" $ do
      calcEnergizedFields field (Coord 0 0) Right `shouldBe` 46

  describe "mostEnergized" $ do
    it "should return 51 for test input" $ do
      let field = readField [ ".|...\\...."
                            , "|.-.\\....."
                            , ".....|-..."
                            , "........|."
                            , ".........."
                            , ".........\\"
                            , "..../.\\\\.."
                            , ".-.-/..|.."
                            , ".|....-|.\\"
                            , "..//.|...."]
      mostEnergized field `shouldBe` 51
