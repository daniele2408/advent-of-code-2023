module FuncsDay2 where

import Text.Show.Functions
import Data.List.Split
import Funcs
import CommonFuncs

--
data CubeGameWrapped = CubeGameWrapped {
  gameId :: Int,
  cubeGame :: CubeGame
} deriving (Eq, Show)

data CubeGame = CubeGame {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Eq, Show, Ord)

type GameSet = [String]

-- | The 'emptyCubeGame' function returns an empty instance of CubeGame
emptyCubeGame :: CubeGame
emptyCubeGame = CubeGame 0 0 0

-- | The 'mergeRollToGame' function applies rolls to a CubeGame
mergeRollToGame :: CubeGame -> String -> CubeGame
mergeRollToGame cg s
  | parsedCommand !! 1 == "red" = mergeRed cg (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "green" = mergeGreen cg (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "blue" = mergeBlue cg (convertStrToInt $ parsedCommand !! 0)
  | otherwise = cg
  where parsedCommand = words s

-- | The 'mergeRed' function generate a new CubeGame having value n for the proper field iff greater of its current value
mergeRed :: CubeGame -> Int -> CubeGame
mergeRed cg n = CubeGame {red=max n (red cg), green=(green cg), blue=(blue cg)}

-- | The 'mergeGreen' function generate a new CubeGame having value n for the proper field iff greater of its current value
mergeGreen :: CubeGame -> Int -> CubeGame
mergeGreen cg n = CubeGame {red=(red cg), green=max n (green cg), blue=(blue cg)}

-- | The 'mergeBlue' function generate a new CubeGame having value n for the proper field iff greater of its current value
mergeBlue :: CubeGame -> Int -> CubeGame
mergeBlue cg n = CubeGame {red=(red cg), green=(green cg), blue=max n (blue cg)}

-- | The 'mergeCubeGame' merges two CubeGames applying merge* for every field
mergeCubeGame :: CubeGame -> CubeGame -> CubeGame
mergeCubeGame cg cg' = mergeBlue (mergeGreen (mergeRed cg (red cg')) (green cg')) (blue cg')

-- | The 'parseLineInput' function extracts a String content written after a colon
parseLineInput :: String -> String
parseLineInput l = head $ tail $ splitStringByAndStripWhiteSpaces ":" l

-- | The 'parseLineGameSetInput' function extracts a String content written after a colon
parseLineGameSetInput :: String -> GameSet
parseLineGameSetInput l = splitStringByAndStripWhiteSpaces ";" l

-- | The parseLineGameId function extracts the integer game id written before the colon
parseLineGameId :: String -> Int
parseLineGameId l = convertStrToInt $ (\x -> x !! 1) $ words $ head $ splitStringByAndStripWhiteSpaces ":" l

-- | The 'isGamePossibleFor' function checks if first CubeGame is possible given the second one
isGamePossibleFor :: CubeGame -> CubeGame -> Bool
isGamePossibleFor c c' = c <= c'

-- | The 'filterGamesBy' function filters wrapped CubeGames, keeping the possible ones given first CubeGame
filterGamesBy :: CubeGame -> [CubeGameWrapped] -> [CubeGameWrapped]
filterGamesBy cg cgws = filter (\cgw -> isGamePossibleFor (cubeGame cgw) cg) cgws

-- | The 'filterGamesBy' function checks if a pool is possible given a CubeGame
-- for example 10 red wouldn't be possible given a CubeGame 1 1 1
isPoolLegal :: CubeGame -> String -> Bool
isPoolLegal cg s
  | parsedCommand !! 1 == "red" = (red cg) >= (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "green" = (green cg) >= (convertStrToInt $ parsedCommand !! 0)
  | parsedCommand !! 1 == "blue" = (blue cg) >= (convertStrToInt $ parsedCommand !! 0)
  | otherwise = False
  where parsedCommand = words s

-- | The 'filterGamesBy' function checks if all pools in a list are legal
arePoolsLegal :: CubeGame -> GameSet -> Bool
arePoolsLegal cg gs = all (\x -> isPoolLegal cg x) gs

-- | The 'areGameSetsLegal' breaks down a line in game sets, checking the legality of each one
areGameSetsLegal :: CubeGame -> String -> Bool
areGameSetsLegal cg s = all (\xs -> arePoolsLegal cg xs) $ parseGameSets s

-- | The 'parseGameSets' function parses a line in a list of game sets,
-- where a game set is a list of pools as "1 red", "4 blue", etc...
parseGameSets :: String -> [GameSet]
parseGameSets s = map (\x -> splitStringByAndStripWhiteSpaces "," x) $ parseLineGameSetInput $ parseLineInput s

-- | The 'powerCubeGame' function computes the power of a CubeGame
powerCubeGame :: CubeGame -> Int
powerCubeGame cg = foldl (\acc x -> acc * x) 1 [(red cg), (green cg), (blue cg)]

-- | The 'findPossibleCubeGameGivenGameSets' function computes for a list of game sets the smallest CubeGame that could be a fit for them
findPossibleCubeGameGivenGameSets :: [GameSet] -> CubeGame
findPossibleCubeGameGivenGameSets gss = foldl (\acc x -> mergeCubeGame acc x) emptyCubeGame (map (\gs -> mergeGameSet gs) gss)

-- | The 'mergeGameSet' compute the smallest CubeGame for a game set
mergeGameSet :: GameSet -> CubeGame
mergeGameSet xs = foldl (\acc x -> mergeRollToGame acc x) emptyCubeGame xs

answerQuestionDayTwo :: String -> Int
answerQuestionDayTwo inputText = sum $ map (\x -> parseLineGameId x) $ filter (\l -> areGameSetsLegal (CubeGame 12 13 14) l) $ lines inputText

-- | The 'parseGameSetsFromFile' function parse a string content, extracting the list of possible CubeGames
parseGameSetsFromFile :: String -> [CubeGame]
parseGameSetsFromFile inputText = map (\gss -> findPossibleCubeGameGivenGameSets gss) $ map (\l -> parseGameSets l) $ lines inputText

answerQuestionDayTwo' :: String -> Int
answerQuestionDayTwo' inputText = foldr (\acc x -> acc + x) 0 $ map (\cg -> powerCubeGame cg) $ parseGameSetsFromFile inputText