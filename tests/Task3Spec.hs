module Task3Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Task3

spec :: Spec
spec = do
  describe "isSymbol" $ do
    it "returns False for 0" $ do
      isSymbol '0' `shouldBe` False
    it "returns False for 1" $ do
      isSymbol '1' `shouldBe` False
    it "returns False for 9" $ do
      isSymbol '9' `shouldBe` False
    it "returns False for ." $ do
      isSymbol '.' `shouldBe` False
    it "returns True for #" $ do
      isSymbol '#' `shouldBe` True
    it "returns True for $" $ do
      isSymbol '$' `shouldBe` True

  describe "neighbors" $ do
    it "returns True for (0, 0, 0) and (0, 0, 1)" $ do
      neighbors (Coord 0 0 0) (Coord 0 0 1) `shouldBe` True
    it "returns True for (0, 0, 0) and (1, 1, 1)" $ do
      neighbors (Coord 0 0 0) (Coord 1 1 1) `shouldBe` True
    it "returns False for (0, 0, 0) and (2, 2, 0)" $ do
      neighbors (Coord 0 0 0) (Coord 2 2 0) `shouldBe` False
    it "returns True for (1, 3, 2) and (0, 0, 1)" $ do
      neighbors (Coord 1 3 2) (Coord 0 0 1) `shouldBe` True
    it "return True for (10, 20, 5) and (13, 15, 6)" $ do
      neighbors (Coord 10 20 5) (Coord 13 15 6) `shouldBe` True

  describe "readSymbols" $ do
    it "returns coords for '...$.*....'" $ do
      readSymbols 2 "...$.*...." `shouldBe` [Coord 3 3 2, Coord 5 5 2]
    it "retruns [] for '....34....'" $ do
      readSymbols 0 "....34...." `shouldBe` []

  describe "readEngineSymbols" $ do
    let input = [ "467..114.."
                , "...*......"
                , "..35..633."
                , "......#..."]
    it "returns coords for input" $ do
      readEngineSymbols input `shouldBe` [Coord 3 3 1, Coord 6 6 3]

  describe "readPartNumber" $ do
    it "returns coords for '467....4..'" $ do
      readPartNumber 0 "467....4.." `shouldBe` [PartNumber 467 (Coord 0 2 0), PartNumber 4 (Coord 7 7 0)]
    it "returns coords for '..50..9..42'" $ do
      readPartNumber 1 "..50..9..42" `shouldBe` [PartNumber 50 (Coord 2 3 1)
                                                , PartNumber 9 (Coord 6 6 1)
                                                , PartNumber 42 (Coord 9 10 1)]
    it "returns [] for '...$.*....'" $ do
      readPartNumber 2 "...$.*...." `shouldBe` []
    it "returns coords for '.....+.58.'" $ do
      readPartNumber 3 ".....+.58." `shouldBe` [PartNumber 58 (Coord 7 8 3)]

  describe "readEnginePartNumbers" $ do
    let input = [ "467..114.."
                , "...*......"
                , "..35..633."
                , "......#..."]
    it "returns coords for input" $ do
      readEnginePartNumbers input `shouldBe` [PartNumber 467 (Coord 0 2 0)
                                             , PartNumber 114 (Coord 5 7 0)
                                             , PartNumber 35 (Coord 2 3 2)
                                             , PartNumber 633 (Coord 6 8 2)]

  describe "isValidPartNumber" $ do
    it "returns True for (Coord 0 2 0) and [(Coord 3 3 2), (Coord 5 5 2)]" $ do
      isValidPartNumber (PartNumber 467 (Coord 0 2 0)) [Coord 3 3 1, Coord 5 5 2] `shouldBe` True
    it "returns False for (Coord 0 2 0) and [(Coord 3 3 2), (Coord 5 5 2)]" $ do
      isValidPartNumber (PartNumber 467 (Coord 0 2 0)) [Coord 3 3 2, Coord 5 5 2] `shouldBe` False

  describe "sumValidPartNumbers" $ do
    let input = [ "467..114.."
                , "...*......"
                , "..35..633."
                , "......#..."
                , "617*......"
                , ".....+.58."
                , "..592....."
                , "......755."
                , "...$.*...."
                , ".664.598.."]
    it "returns 4361 for input" $ do
      sumValidPartNumbers input `shouldBe` 4361

  describe "readEngineGearSymbols" $ do
    let input = [ "467..114.."
                , "...*......"
                , "..35..633."]
    it "returns coords for input" $ do
      readEngineGearSymbols input `shouldBe` [Coord 3 3 1]

  describe "validGearNeghbors" $ do
    let partNumbers = [ PartNumber 467 (Coord 0 2 0)
                      , PartNumber 114 (Coord 5 7 0)
                      , PartNumber 35 (Coord 2 3 2)
                      , PartNumber 617 (Coord 0 2 4)]
    it "returns neighbors for (Coord 3 3 1)" $ do
      validGearNeghbors (Coord 3 3 1) partNumbers `shouldBe` Just (PartNumber 467 (Coord 0 2 0), PartNumber 35 (Coord 2 3 2))
    it "returns [] for (Coord 3 3 4)" $ do
      validGearNeghbors (Coord 3 3 4) partNumbers `shouldBe` Nothing

  describe "gearRatio" $ do
    it "returns 0 for Nothing" $ do
      gearRatio Nothing `shouldBe` 0
    it "returns 467 * 35 for Just (PartNumber 467 (Coord 0 2 0), PartNumber 35 (Coord 2 3 2))" $ do
      gearRatio (Just (PartNumber 467 (Coord 0 2 0), PartNumber 35 (Coord 2 3 2))) `shouldBe` 467 * 35

  describe "sumGearRatios" $ do
    let input = [ "467..114.."
            , "...*......"
            , "..35..633."
            , "......#..."
            , "617*......"
            , ".....+.58."
            , "..592....."
            , "......755."
            , "...$.*...."
            , ".664.598.."]
    it "returns 467835 for input" $ do
      sumGearRatios input `shouldBe` 467835
