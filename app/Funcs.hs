module Funcs where

import Data.Char
import Data.List
import Data.Maybe


numbers :: [String]
numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- |The 'convertStrToInt' function converts a string in integer if all chars are digits, otherwise we'll get an error
convertStrToInt :: String -> Int
convertStrToInt s
    | s == "" = 0
    | all (isDigit) s = read s :: Int
    | otherwise = error "Not a number"

-- |The 'extractFirstDigit' function returns first numerical char from a string,
-- for example will return 5 from "acbfd56e7wq"
extractFirstDigit :: String -> Char
extractFirstDigit s = head $ filter (isDigit) s

-- |The 'extractLastDigit' function returns last numerical char from a string,
-- for example will return 7 from "acbfd56e7wq"
extractLastDigit :: String -> Char
extractLastDigit s = extractFirstDigit $ reverse s

-- |The 'extractDigits' function returns an integer composed concatenating chars from extract*Digit functions,
--  for example will return 57 from "acbfd56e7wq"
extractDigits :: String -> Int
extractDigits s = convertStrToInt $ ((extractFirstDigit s) : (extractLastDigit s) : [])

-- |The 'retrieveIndexIterative' function returns index position of a substring s in a string (x:xs), or -1 if absent
retrieveIndexIterative :: String -> String -> Int -> Int
retrieveIndexIterative s (x:xs) pos
    | (&&) ((x:xs) == [x]) ((x:xs) == s) = pos
    | (x:xs) == [x] = -1
    | (x:xs) == "" = -1
    | isPrefixOf s (x:xs) = pos
    | otherwise = retrieveIndexIterative s xs (pos+1)

-- |The 'rankStrings' function ranks a list of string xs in terms of index position for each string in string s
-- expressed as a list of tuple (string, position)
rankStrings :: [String] -> String -> [(String, Int)]
rankStrings xs s = filter (\x -> (snd x) >= 0) $ map (\x -> (x, retrieveIndexIterative x s 0)) xs

-- |The 'findMinTupleBySnd' will single out the string being in the tuple storing the smallest value in second position
findMinTupleBySnd :: [(String, Int)] -> String
findMinTupleBySnd [] = ""
findMinTupleBySnd (x:xs)
    | xs == [] = fst x
    | otherwise = findMinTupleBySnd $ (compareTuples x (head xs)):(tail xs)

-- |The 'findMaxTupleBySnd' will single out the string being in the tuple storing the greatest value in second position
findMaxTupleBySnd :: [(String, Int)] -> String
findMaxTupleBySnd [] = ""
findMaxTupleBySnd (x:xs)
    | xs == [] = fst x
    | otherwise = findMaxTupleBySnd $ (compareTuples (head xs) x):(tail xs)

-- |The 'compareTuples' will select among two tuples the one having the smallest value
compareTuples :: (String, Int) -> (String, Int) -> (String, Int)
compareTuples (f,s) (f',s')
    | s <= s' = (f,s)
    | otherwise = (f',s')    

-- |The 'readNumber' function will convert a worded number as its numeric equivalent
readNumber :: String -> String
readNumber n
    | n == "one" = "1"
    | n == "two" = "2"
    | n == "three" = "3"
    | n == "four" = "4"
    | n == "five" = "5"
    | n == "six" = "6"
    | n == "seven" = "7"
    | n == "eight" = "8"
    | n == "nine" = "9"
    | otherwise = n

-- |The 'extractFirstNumber' function works like 'extractFirstDigit', but supporting number words as well
extractFirstNumber :: String -> String
extractFirstNumber s = readNumber $ findMinTupleBySnd $ rankStrings numbers s

-- |The 'extractLastNumber' function works like 'extractLastDigit', but supporting number words as well
extractLastNumber :: String -> String
extractLastNumber s = readNumber $ reverse $ findMinTupleBySnd $ rankStrings (map (reverse) numbers) (reverse s)

-- |The 'extractFirstAndLastNumber' function works like 'extractFirstAndLastDigit', but supporting number words as well
extractFirstAndLastNumber :: String -> Int
extractFirstAndLastNumber s = convertStrToInt $ ((head $ extractFirstNumber s) : (head $ extractLastNumber s) : [])

answerQuestionDayOne :: String -> Int
answerQuestionDayOne inputText = sum $ map (\l -> extractDigits l) $ lines inputText

answerQuestionDayOne' :: String -> Int
answerQuestionDayOne' inputText = sum $ map (\l -> extractFirstAndLastNumber l) $ lines inputText