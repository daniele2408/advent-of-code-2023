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
updateScratchCardHolderCopies idSc n sch
  | oldNCopies == [] = sch
  | otherwise = ((idSc,(snd $ head $ oldNCopies)+n): (filter (\sc -> (fst sc) /= idSc ) sch))
    where oldNCopies = take 1 (filter (\sc -> (fst sc) == idSc ) sch)

generateUpdateCommands :: ScratchCard -> Int -> [(Int, Int)]
generateUpdateCommands sc n
  | nMatches == 0 = []
  | nMatches < 0 = error "Can't have a negative number of matches"
  | otherwise = zip [(scId+1)..(scId+nMatches)] (repeat n)
  where nMatches = length $ getMatchingNumbers sc
        scId = idScratchCard sc

applyUpdateCommands :: [(Int, Int)] -> ScratchCardHolder -> ScratchCardHolder
applyUpdateCommands [] acc = acc
applyUpdateCommands (c:cs) acc = applyUpdateCommands cs (updateScratchCardHolderCopies (fst c) (snd c) acc)

updateCopiesIterative :: [ScratchCard] -> ScratchCardHolder -> ScratchCardHolder
updateCopiesIterative [] acc = acc
updateCopiesIterative (x:xs) acc = updateCopiesIterative xs (applyUpdateCommands (generateUpdateCommands x (extractNCopies x acc)) acc)

extractNCopies :: ScratchCard -> ScratchCardHolder -> Int
extractNCopies sc sch
  | pair == [] = 0
  | otherwise = (snd $ head pair)
  where pair = take 1 $ filter (\p -> (idScratchCard sc) == (fst p)) sch

convertLinesToScratchCards :: [String] -> [ScratchCard]
convertLinesToScratchCards xs = map (\l -> parseScratchCardFromLine l) xs

initScratchCardHolderFrom :: [ScratchCard] -> ScratchCardHolder
initScratchCardHolderFrom xs = zip [1..(length xs)] (repeat 1)

answerQuestionDayFour :: String -> Int
answerQuestionDayFour inputText = sum $ map (\sc -> computeCardValue sc) $ convertLinesToScratchCards $ lines inputText

answerQuestionDayFour' :: String -> Int
answerQuestionDayFour' inputText = sum $ map (\pair -> (snd pair)) $ updateCopiesIterative scratchCards (initScratchCardHolderFrom scratchCards)
  where scratchCards = convertLinesToScratchCards $ lines inputText