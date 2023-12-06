module FuncsDay6 where

import Data.Char
import Data.List
import CommonFuncs

type RaceRecord = (Int, Int) -- time, distance

parseTimesFromInputText :: String -> [Int]
parseTimesFromInputText inputText = map (\w -> convertStrToInt w) $ words $ selectDataPart timesLine
    where ls = lines inputText
          timesLine = ls !! 0

removeWhiteSpaces :: String -> String
removeWhiteSpaces s
    | s == [] = s
    | otherwise = filter (\c -> c /= ' ') s

parseDistancesFromInputText :: String -> [Int]
parseDistancesFromInputText inputText = map (\w -> convertStrToInt w) $ words $ selectDataPart distancesLine
    where ls = lines inputText
          distancesLine = ls !! 1

selectDataPart :: String -> String
selectDataPart s = (splitStringByAndStripWhiteSpaces ":" s) !! 1

computeTravelIfHolding :: Int -> Int -> RaceRecord
computeTravelIfHolding 0 maxTime = (maxTime, 0)
computeTravelIfHolding ms maxTime
    | ms == maxTime = (maxTime, 0)
    | otherwise = (maxTime, (computeDistance ms timeLeft))
        where timeLeft = maxTime - ms

computeDistance :: Int -> Int -> Int
computeDistance speed time = speed * time

findBetterStrategies :: RaceRecord -> [RaceRecord]
findBetterStrategies rr = filter (\rr' -> (snd rr') > distance) $ map (\holdTime -> computeTravelIfHolding holdTime time) [0..time]
    where time = fst rr
          distance = snd rr

parseRaceRecords :: String -> [RaceRecord]
parseRaceRecords inputText = zip (parseTimesFromInputText inputText) (parseDistancesFromInputText inputText)

productList :: [Int] -> Int
productList [] = 0
productList xs = foldl (\acc x -> acc * x) 1 xs

answerQuestionDaySix :: String -> Int
answerQuestionDaySix inputText = productList $ map (\rr -> length $ findBetterStrategies rr) $ parseRaceRecords inputText

parseDistancesFromInputText' :: String -> Int
parseDistancesFromInputText' inputText = convertStrToInt $ removeWhiteSpaces $ selectDataPart distancesLine
    where ls = lines inputText
          distancesLine = ls !! 1

parseTimesFromInputText' :: String -> Int
parseTimesFromInputText' inputText = convertStrToInt $ removeWhiteSpaces $ selectDataPart timesLine
    where ls = lines inputText
          timesLine = ls !! 0

parseRaceRecord :: String -> RaceRecord
parseRaceRecord inputText = ((parseTimesFromInputText' inputText), (parseDistancesFromInputText' inputText))

answerQuestionDaySix' :: String -> Int
answerQuestionDaySix' inputText = length $ findBetterStrategies $ parseRaceRecord inputText
