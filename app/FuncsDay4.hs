module FuncsDay4 where

import Data.Char

import CommonFuncs

type ScratchCardHolder = [(Int, Int)] -- id, nCopies

data ScratchCard = ScratchCard {
    idScratchCard :: Int,
    winningNumbers :: [Int],
    scratchedNumbers :: [Int]
} deriving (Show, Eq)

parseScratchCardFromLine :: String -> ScratchCard
parseScratchCardFromLine l = ScratchCard (parseIdScratchCardFromSplitted $ splittedLine !! 0) (parseWinningNumbers $ splittedLine !! 1) (parseScratchedNumbers $ splittedLine !! 1)
    where splittedLine = splitStringByAndStripWhiteSpaces ":" l

parseIdScratchCardFromSplitted :: String -> Int
parseIdScratchCardFromSplitted s = convertStrToInt $ (words s) !! 1

parseWinningNumbers :: String -> [Int]
parseWinningNumbers numberParts = map (\w -> convertStrToInt w) $ words $ (splitStringByAndStripWhiteSpaces "|" numberParts) !! 0

parseScratchedNumbers :: String -> [Int]
parseScratchedNumbers numberParts = map (\w -> convertStrToInt w) $ words $ (splitStringByAndStripWhiteSpaces "|" numberParts) !! 1

isContained :: Int -> [Int] -> Bool
isContained i is = any (\i' -> i' == i) is

computeCardValue :: ScratchCard -> Int
computeCardValue sc
    | matches >= 1 = 2 ^ (matches-1)
    | otherwise = 0
    where matches = length $ getMatchingNumbers sc

getMatchingNumbers :: ScratchCard -> [Int]
getMatchingNumbers sc = filter (\sn -> isContained sn (winningNumbers sc)) (scratchedNumbers sc)

updateScratchCardHolderCopies :: Int -> Int -> ScratchCardHolder -> ScratchCardHolder
updateScratchCardHolderCopies idSc n sch = ((idSc,oldNCopies+1): (filter (\sc -> (fst sc) /= idSc ) sch))
    where oldNCopies = snd $ head $ take 1 (filter (\sc -> (fst sc) == idSc ) sch)

answerQuestionDayFour :: String -> Int
answerQuestionDayFour inputText = sum $ map (\sc -> computeCardValue sc) $ map (\l -> parseScratchCardFromLine l) $ lines inputText
