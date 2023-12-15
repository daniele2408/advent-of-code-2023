module CommonFuncs where

import Data.List.Split
import Data.Char
import Data.List

isStringAllDigits :: String -> Bool
isStringAllDigits s = all (isDigit) s

-- |The 'convertStrToInt' function converts a string in integer if all chars are digits, otherwise we'll get an error
convertStrToInt :: String -> Int
convertStrToInt s
    | s == "" = 0
    | all (isDigit) unsigned = applySign s
    | otherwise = error "Not a number"
        where unsigned = stripCharacter s '-'

applySign :: String -> Int
applySign s
    | (head s) == '-' = -1 * i
    | otherwise = i
    where i = read (stripCharacter s '-') :: Int

-- |The 'retrieveIndexIterative' function returns index position of a substring s in a string (x:xs), or -1 if absent
retrieveIndexIterative :: String -> String -> Int -> Int
retrieveIndexIterative s (x:xs) pos
    | (&&) ((x:xs) == [x]) ((x:xs) == s) = pos
    | (x:xs) == [x] = -1
    | (x:xs) == "" = -1
    | isPrefixOf s (x:xs) = pos
    | otherwise = retrieveIndexIterative s xs (pos+1)

-- | The 'splitStringByAndStripWhiteSpaces' split a string using the designated separator and remove trailing and leading
-- whitespaces
splitStringByAndStripWhiteSpaces :: String -> String -> [String]
splitStringByAndStripWhiteSpaces sep s = map (\x -> stripWhiteSpaces x) $ splitOn sep s

-- | The 'stripWhiteSpaces' functions strips whitespaces from a string
stripWhiteSpaces :: String -> String
stripWhiteSpaces [] = []
stripWhiteSpaces (x:[]) = [x]
stripWhiteSpaces (x:xs)
  | (&&) ((head $ reverse xs) == ' ') (x == ' ') = reverse $ tail $ reverse xs
  | x == ' ' = xs
  | (head $ reverse xs) == ' ' = (x: (reverse $ tail $ reverse xs))
  | otherwise = (x:xs)

stripCharacter :: String -> Char -> String
stripCharacter [] _ = []
stripCharacter (x:[]) _ = [x]
stripCharacter (x:xs) c
  | (&&) ((head $ reverse xs) == c) (x == c) = reverse $ tail $ reverse xs
  | x == c = xs
  | (head $ reverse xs) == c = (x: (reverse $ tail $ reverse xs))
  | otherwise = (x:xs)

extractOddPosElements :: [a] -> [a]
extractOddPosElements [] = []
extractOddPosElements xs = map (\p -> snd p) $ filter (\p -> (mod (fst p) 2) == 1) $ zip [0..(l-1)] xs
    where l = length xs

extractEvenPosElements :: [a] -> [a]
extractEvenPosElements [] = []
extractEvenPosElements xs = map (\p -> snd p) $ filter (\p -> (mod (fst p) 2) == 0) $ zip [0..(l-1)] xs
    where l = length xs

sortPairsBySecond (_, a1) (_, a2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | a1 == a2 = EQ

sortPairsByFirst (a1, _) (a2, _)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | a1 == a2 = EQ

-- auxiliary, it filters and extracts non-empty Maybe values in a Maybes list
accumulateJustsFromMaybes :: [Maybe a] -> [a] -> [a]
accumulateJustsFromMaybes [] acc = acc
accumulateJustsFromMaybes ((Just x):xs) acc = accumulateJustsFromMaybes xs (x:acc)
accumulateJustsFromMaybes (Nothing:xs) acc = accumulateJustsFromMaybes xs acc

joinStrings :: [String] -> Char -> String -> String
joinStrings [] c acc = acc
joinStrings (x:xs) c acc = joinStrings xs c (acc ++ x ++ [c])