module Task15Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task15 ( hashChar
              , hashStr
              , sumOfHashes
              , splitByOneOf
              , Command(..)
              , Lens(..)
              , Box (..)
              , createBoxes
              , command
              , boxFocusingPower
              , sumOfFocusingPower)
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "hashChar" $ do
    it "should return 200 for H" $ do
      hashChar 0 'H' `shouldBe` 200

  describe "hashStr" $ do
    it "should return 52 for HASH" $ do
      hashStr "HASH" `shouldBe` 52
    it "should return 0 for cm" $ do
      hashStr "cm" `shouldBe` 0

  describe "sumOfHashes" $ do
    it "should return 1320 for test example" $ do
      sumOfHashes "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" `shouldBe` 1320

  describe "splitByOneOf" $ do
    let delimeters = "=-"
    it "should split 'rn=1'" $ do
      splitByOneOf delimeters "rn=1" `shouldBe` ["rn", "=", "1"]
    it "should split 'cm-'" $ do
      splitByOneOf delimeters "cm-" `shouldBe` ["cm", "-", ""]

  describe "Command" $ do
    it "should read 'rn=1'" $ do
      read "rn=1" `shouldBe` AddLens (Lens "rn" 1)
    it "should read 'cm-'" $ do
      read "cm-" `shouldBe` RemoveLens "cm"

  describe "command" $ do
    it "should add lens to box 0 for rn=1" $ do
      let boxes = createBoxes 4
      command boxes (AddLens (Lens "rn" 1)) `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 1])
                                                                    , (1, Box 1 [])
                                                                    , (2, Box 2 [])
                                                                    , (3, Box 3 []) ]

    it "should remove lens if exists from box 0 for cm-" $ do
      let boxes = Map.fromList [ (0, Box 0 [Lens "rn" 1])
                               , (1, Box 1 [])
                               , (2, Box 2 [])
                               , (3, Box 3 []) ]
      command boxes (RemoveLens "cm") `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 1])
                                                              , (1, Box 1 [])
                                                              , (2, Box 2 [])
                                                              , (3, Box 3 []) ]
    it "should add lens to box 1 for qp=3" $ do
      let boxes = Map.fromList [ (0, Box 0 [Lens "rn" 1])
                               , (1, Box 1 [])
                               , (2, Box 2 [])
                               , (3, Box 3 []) ]
      command boxes (AddLens (Lens "qp" 3)) `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 1])
                                                                    , (1, Box 1 [Lens "qp" 3])
                                                                    , (2, Box 2 [])
                                                                    , (3, Box 3 []) ]
    it "should add lens to box 0 for cm=2" $ do
      let boxes = Map.fromList [ (0, Box 0 [Lens "rn" 1])
                               , (1, Box 1 [Lens "qp" 3])
                               , (2, Box 2 [])
                               , (3, Box 3 []) ]
      command boxes (AddLens (Lens "cm" 2)) `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 1, Lens "cm" 2])
                                                                    , (1, Box 1 [Lens "qp" 3])
                                                                    , (2, Box 2 [])
                                                                    , (3, Box 3 []) ]
    it "should remove lens from box 1 for qp-" $ do
      let boxes = Map.fromList [ (0, Box 0 [Lens "rn" 1, Lens "cm" 2])
                               , (1, Box 1 [Lens "qp" 3])
                               , (2, Box 2 [])
                               , (3, Box 3 []) ]
      command boxes (RemoveLens "qp") `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 1, Lens "cm" 2])
                                                              , (1, Box 1 [])
                                                              , (2, Box 2 [])
                                                              , (3, Box 3 []) ]
    it "should replace existing lens for rn=4" $ do
      let boxes = Map.fromList [ (0, Box 0 [Lens "rn" 1, Lens "cm" 2])
                               , (1, Box 1 [])
                               , (2, Box 2 [])
                               , (3, Box 3 []) ]
      command boxes (AddLens (Lens "rn" 4)) `shouldBe` Map.fromList [ (0, Box 0 [Lens "rn" 4, Lens "cm" 2])
                                                                    , (1, Box 1 [])
                                                                    , (2, Box 2 [])
                                                                    , (3, Box 3 []) ]

  describe "boxFocusingPower" $ do
    it "should return 5 for Box 0: [rn 1] [cm 2]" $ do
      boxFocusingPower (Box 0 [Lens "rn" 1, Lens "cm" 2]) `shouldBe` 5

  describe "sumOfFocusingPower" $ do
    it "should return 145 for test example" $ do
      let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
      sumOfFocusingPower input `shouldBe` 145
