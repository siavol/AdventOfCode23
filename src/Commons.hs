module Commons where

-- reads input from file
readInput :: FilePath -> IO [String]
readInput filename = do
  contents <- readFile filename
  return (lines contents)

trimStart :: String -> String
trimStart = dropWhile (== ' ')

isDigit :: Char -> Bool
isDigit c = c `elem` ['0'..'9']

-- returns number and rest of the string after title
-- like "<name> <number: <rest"
-- f.e "Game N: 2 blue, 3 red; 3 green, 3 blue, 6 red;"
readTitledRecord :: String -> (Int, String)
readTitledRecord str = go str []
  where
    go :: String -> String -> (Int, String)
    go [] accNum = error "unexpected end of the string"
    go (x:xs) accNum
      | x == ':' = (read accNum, trimStart xs)
      | isDigit x = go xs (accNum ++ [x])
      | otherwise = go xs accNum

splitBy :: Char -> String -> [String]
splitBy c str = go str [] []
  where
    go :: String -> String -> [String] -> [String]
    go [] [] res = res
    go [] acc res = (res ++ [acc])
    go (x:xs) acc res
      | x == c = go xs [] (res ++ [acc])
      | otherwise = go xs (acc ++ [x]) res
