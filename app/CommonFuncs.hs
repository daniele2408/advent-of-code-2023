module CommonFuncs where

import Data.Char
import Data.List

isStringAllDigits :: String -> Bool
isStringAllDigits s = all (isDigit) s

-- |The 'convertStrToInt' function converts a string in integer if all chars are digits, otherwise we'll get an error
convertStrToInt :: String -> Int
convertStrToInt s
    | s == "" = 0
    | all (isDigit) s = read s :: Int
    | otherwise = error "Not a number"

-- |The 'retrieveIndexIterative' function returns index position of a substring s in a string (x:xs), or -1 if absent
retrieveIndexIterative :: String -> String -> Int -> Int
retrieveIndexIterative s (x:xs) pos
    | (&&) ((x:xs) == [x]) ((x:xs) == s) = pos
    | (x:xs) == [x] = -1
    | (x:xs) == "" = -1
    | isPrefixOf s (x:xs) = pos
    | otherwise = retrieveIndexIterative s xs (pos+1)

