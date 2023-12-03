module Task3 where

import Data.List (intersect)

runTask :: IO ()
runTask = do
  print "=== Advent Code 3 ==="
  input <- readInput "./inputs/task-3.txt"
  print $ sumValidPartNumbers input
  print $ sumGearRatios input

-- reads input from file
readInput :: FilePath -> IO [String]
readInput filename = do
  contents <- readFile filename
  return (lines contents)

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

-- checks if char is a symbol.
-- Digits and periods are not symbols.
isSymbol :: Char -> Bool
isSymbol c
    | isDigit c = False
    | c == '.' = False
    | otherwise = True

isGearSymbol :: Char -> Bool
isGearSymbol '*' = True
isGearSymbol _ = False

-- Coordinate of element on the map
data Coord = Coord { xStart :: Int
                   , xEnd :: Int
                   , y :: Int
                   } deriving (Show, Eq)

neighbors :: Coord -> Coord -> Bool
neighbors (Coord xStart1 xEnd1 y1) (Coord xStart2 xEnd2 y2)
    | abs (y1 - y2) > 1 = False
    | length (intersect [xStart1-1..xEnd1+1] [xStart2-1..xEnd2+1]) >= 2 = True
    | otherwise = False

data PartNumber = PartNumber { part :: Int
                             , coord :: Coord
                             } deriving (Show, Eq)

-- reads symbol coords from string like "...$.*...."
readSymbols :: Int -> String -> [Coord]
readSymbols y str = map (\(x, _) -> Coord x x y) $ filter (\(_, c) -> isSymbol c) $ zip [0..] str

readEngineSymbols :: [String] -> [Coord]
readEngineSymbols = concat . map (\(y, str) -> readSymbols y str) . zip [0..]

readGearSymbols :: Int -> String -> [Coord]
readGearSymbols y str = map (\(x, _) -> Coord x x y) $ filter (\(_, c) -> isGearSymbol c) $ zip [0..] str

readEngineGearSymbols :: [String] -> [Coord]
readEngineGearSymbols = concat . map (\(y, str) -> readGearSymbols y str) . zip [0..]

isEmpty :: String -> Bool
isEmpty [] = True
isEmpty _ = False

-- reads part number with coords from string like "467....4.."
readPartNumber :: Int -> String -> [PartNumber]
readPartNumber y str = go 0 str "" []
    where
        go :: Int -> String -> String -> [PartNumber] -> [PartNumber]
        go _ [] [] coords = coords
        go x [] num coords = coords ++ [PartNumber (read num) (Coord (x - length num) (x - 1) y)]
        go x (c:cs) num coords
            | isDigit c = go (x+1) cs (num ++ [c]) coords
            | isEmpty num = go (x+1) cs "" coords
            | otherwise = go (x+1) cs "" (coords ++ [PartNumber (read num) (Coord (x - length num) (x - 1) y)])

readEnginePartNumbers :: [String] -> [PartNumber]
readEnginePartNumbers = concat . map (\(y, str) -> readPartNumber y str) . zip [0..]

-- checks if part number is valid.
-- Valid part number is neighbor of at least one symbol.
isValidPartNumber :: PartNumber -> [Coord] -> Bool
isValidPartNumber (PartNumber _ coord) symbols = any (\s -> neighbors coord s) symbols

sumValidPartNumbers :: [String] -> Int
sumValidPartNumbers input = sum $ map (\(PartNumber part _) -> part) validPartNumbers
    where
        partNumbers = readEnginePartNumbers input
        symbols = readEngineSymbols input
        validPartNumbers = filter (\p -> isValidPartNumber p symbols) partNumbers

-- returns neighbors if gear is valid.
-- Valid gear is neighbor of exactly two part numbers.
validGearNeghbors :: Coord -> [PartNumber] -> Maybe (PartNumber, PartNumber)
validGearNeghbors gear partNumbers = if length neighborParts == 2 then Just (head neighborParts, last neighborParts) else Nothing
  where
    neighborParts = filter (\p -> neighbors gear (coord p)) partNumbers

gearRatio :: Maybe (PartNumber, PartNumber) -> Int
gearRatio Nothing = 0
gearRatio (Just (PartNumber part1 _, PartNumber part2 _)) = part1 * part2

sumGearRatios :: [String] -> Int
sumGearRatios input = sum ratios
  where
    partNumbers = readEnginePartNumbers input
    gearSymbols = readEngineGearSymbols input
    validGears = map (\g -> validGearNeghbors g partNumbers) gearSymbols
    ratios = map gearRatio validGears
