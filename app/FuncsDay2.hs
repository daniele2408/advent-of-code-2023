module FuncsDay2 where

import Text.Show.Functions
import Data.List.Split
import Funcs

data CubeGameWrapped = CubeGameWrapped {
  gameId :: Int,
  cubeGame :: CubeGame
} deriving (Eq, Show)

data CubeGame = CubeGame {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Eq, Show, Ord)

emptyCubeGame :: CubeGame
emptyCubeGame = CubeGame 0 0 0

addRollToGame :: CubeGame -> String -> CubeGame
addRollToGame cg s
  | parsedCommand !! 1 == "red" = CubeGame {red=(red cg)+(convertStrToInt $ parsedCommand !! 0), green=(green cg), blue=(blue cg)}
  | parsedCommand !! 1 == "green" = CubeGame {red=(red cg), green=(green cg)+(convertStrToInt $ parsedCommand !! 0), blue=(blue cg)}
  | parsedCommand !! 1 == "blue" = CubeGame {red=(red cg), green=(green cg), blue=(blue cg)+(convertStrToInt $ parsedCommand !! 0)}
  | otherwise = cg
  where parsedCommand = words s


addRollToGameIterative :: [String] -> CubeGame -> CubeGame
addRollToGameIterative (x:xs) cg
  | (x:xs) == [] = cg
  | otherwise = addRollToGameIterative xs (addRollToGame cg x)
addRollToGameIterative _ cg = cg

splitStringBy :: String -> String -> [String]
splitStringBy sep s = map (\x -> stripWhiteSpaces x) $ splitOn sep s

stripWhiteSpaces :: String -> String
stripWhiteSpaces (x:xs)
  | (&&) ((head $ reverse xs) == ' ') (x == ' ') = reverse $ tail $ reverse xs
  | x == ' ' = xs
  | (head $ reverse xs) == ' ' = (x: (reverse $ tail $ reverse xs))
  | otherwise = (x:xs)


parseGame :: String -> CubeGame
parseGame s = addRollToGameIterative (splitStringBy "," s) emptyCubeGame

parseLineInput :: String -> String
parseLineInput l = head $ tail $ splitStringBy ":" l

parseLineGameSetInput :: String -> [String]
parseLineGameSetInput l = splitStringBy ";" l

parseLineGameId :: String -> Int
parseLineGameId l = convertStrToInt $ (\x -> x !! 1) $ words $ head $ splitStringBy ":" l

parseIdAndGame :: String -> CubeGameWrapped
parseIdAndGame s = CubeGameWrapped (parseLineGameId s) (parseGame $ parseLineInput s)

isGamePossibleFor :: CubeGame -> CubeGame -> Bool
isGamePossibleFor c c' = c <= c'

filterGamesBy :: CubeGame -> [CubeGameWrapped] -> [CubeGameWrapped]
filterGamesBy cg cgws = filter (\cgw -> isGamePossibleFor (cubeGame cgw) cg) cgws

isPoolLegal :: CubeGame -> String -> Bool
isPoolLegal cg s
  | parsedCommand !! 1 == "red" = (red cg) >= (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "green" = (green cg) >= (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "blue" = (blue cg) >= (convertStrToInt $ parsedCommand !! 0)
  | otherwise = False
  where parsedCommand = words s

arePoolsLegal :: CubeGame -> [String] -> Bool
arePoolsLegal cg xs = all (\x -> isPoolLegal cg x) xs

areGameSetsLegal :: CubeGame -> String -> Bool
areGameSetsLegal cg s = all (\xs -> arePoolsLegal cg xs) (map (\x -> splitStringBy "," x) $ parseLineGameSetInput $ parseLineInput s)

answerQuestionDayTwo :: String -> Int
answerQuestionDayTwo inputText = sum $ map (\x -> parseLineGameId x) $ filter (\l -> areGameSetsLegal (CubeGame 12 13 14) l) $ lines inputText