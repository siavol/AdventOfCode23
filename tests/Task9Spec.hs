module Task9Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Prelude hiding (Left, Right)

import Task9

spec :: Spec
spec = do
  describe "difference" $ do
    it "returns difference for 0   3   6   9  12  15" $ do
      difference [0, 3, 6, 9, 12, 15] `shouldBe` [3, 3, 3, 3, 3]
    it "returns difference for 1   3   6  10  15  21" $ do
      difference [1, 3, 6, 10, 15, 21] `shouldBe` [2, 3, 4, 5, 6]

  describe "diffSequnce" $ do
    it "returns difference sequence for 0   3   6   9  12  15" $ do
      diffSequnce [0, 3, 6, 9, 12, 15] `shouldBe` [ [   0, 0, 0,  0    ]
                                                  , [ 3, 3, 3,  3,  3  ]
                                                  , [0, 3, 6, 9, 12, 15]]

  describe "propagateDiff" $ do
    it "propagates difference back for test input" $ do
      let input = [ [   0, 0, 0,  0    ]
                  , [ 3, 3, 3,  3,  3  ]
                  , [0, 3, 6, 9, 12, 15]]
      propagateDiff input `shouldBe` [-3, 0, 3, 6, 9, 12, 15, 18]
    it "propagates difference back for test input 3" $ do
      let input = [ [          0,   0           ]
                  , [       2,   2,   2         ]
                  , [     0,   2,   4,   6      ]
                  , [   3,   3,   5,   9,  15   ]
                  , [10,  13,  16,  21,  30,  45]]
      propagateDiff input `shouldBe` [5, 10, 13, 16, 21, 30, 45, 68]

  describe "valuesHistory" $ do
    it "reads test input" $ do
      let input = [ "0 3 6 9 12 15"
                  , "1 3 6 10 15 21"
                  , "10 13 16 21 30 45"]
      valuesHistory input `shouldBe` [ [0, 3, 6, 9, 12, 15]
                                     , [1, 3, 6, 10, 15, 21]
                                     , [10, 13, 16, 21, 30, 45]]

  describe "sumOfExtrapolatedLasts" $ do
    it "returns sum of extrapolated numbers for test input" $ do
      let input = [ "0 3 6 9 12 15"
                  , "1 3 6 10 15 21"
                  , "10 13 16 21 30 45"]
      sumOfExtrapolatedLasts input `shouldBe` 114
