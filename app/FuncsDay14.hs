{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay14 where

import CommonFuncs
import Data.List
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

data TileContent = R | C | V deriving (Show, Eq)
data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq)
data Tile = Tile { content :: TileContent, coords :: Coords } deriving (Show, Eq)
type Row = [Tile]
type Grid = [Row]

instance Show Row where
  show r = foldl (\acc x -> acc ++ x) [] [(map (\d -> contentAsChar $ content d) r)]

instance Show Grid where
  show g = joinStrings (map (\d -> show d) g) '\n' ""

data Direction = N | S | W | E deriving (Show, Eq)

findFirstEmptySpace :: Grid -> Direction -> Tile -> Maybe Coords
findFirstEmptySpace g d startTile
  | d == W = takeMaybeLast $ takeWhile (\c -> isEmpty $ getTile g c) $ tail $ [Coords {x = x, y = ty} | x <- [tx, (tx-1) .. 0]]
  | d == E = takeMaybeLast $ takeWhile (\c -> isEmpty $ getTile g c) $ tail $ [Coords {x = x, y = ty} | x <- [tx .. height]]
  | d == N = takeMaybeLast $ takeWhile (\c -> isEmpty $ getTile g c) $ tail $ [Coords {x = tx, y = y} | y <- [ty, (ty-1) .. 0]]
  | d == S = takeMaybeLast $ takeWhile (\c -> isEmpty $ getTile g c) $ tail $ [Coords {x = tx, y = y} | y <- [ty .. width]]
  where tx = x (coords startTile)
        ty = y (coords startTile)
        height = (length g) - 1
        width = (length $ g !! 0) - 1

getTile :: Grid -> Coords -> Maybe Tile
getTile g cs
  | (||) (cx >= width) (cy >= height) = Nothing
  | otherwise = Just $ (!! cx) $ g !! cy
  where width = length $ g !! 0
        height = length $ g
        cx = x cs
        cy = y cs

takeMaybeLast :: [a] -> Maybe a
takeMaybeLast [] = Nothing
takeMaybeLast (x:[]) = Just x
takeMaybeLast xs = Just $ head $ reverse xs

isEmpty :: Maybe Tile -> Bool
isEmpty Nothing = False
isEmpty (Just t) = (content t) == V

findAllRoundedRocks :: Grid -> [Tile]
findAllRoundedRocks g = filter (\t -> (== R) $ content t) $ concat g

moveAllRocks :: Direction -> Grid -> Grid
moveAllRocks d g = moveAllRocksIterative g d rocks
  where rocks = findAllRounderRocksOrderedBy g d

findAllRounderRocksOrderedBy :: Grid -> Direction -> [Tile]
findAllRounderRocksOrderedBy g d
  | d == N = findAllRoundedRocks g
  | d == S = findAllRoundedRocks $ reverse g
  | d == W = findAllRoundedRocks $ transpose g
  | d == E = findAllRoundedRocks $ reverse $ transpose g
  | otherwise = []

moveAllRocksIterative :: Grid -> Direction -> [Tile] -> Grid
moveAllRocksIterative g d [] = g
moveAllRocksIterative g d (x:xs) = moveAllRocksIterative newGrid d xs
  where newGrid = move g d x

move :: Grid -> Direction -> Tile -> Grid
move g d t =
  case maybeEmptySpaceCoords of
     Nothing -> g
     Just cs -> vaporizeRock (replaceTileInGridAt g cs t) t
  where maybeEmptySpaceCoords = findFirstEmptySpace g d t

replaceTileInGridAt :: Grid -> Coords -> Tile -> Grid
replaceTileInGridAt g cs t = replaceRow g rowToReplace $ replaceTile rowToReplace colPos rowPos t
  where rowPos = y cs
        colPos = x cs
        rowToReplace = g !! rowPos

vaporizeRock :: Grid -> Tile -> Grid
vaporizeRock g t = replaceTileInGridAt g cs Tile { content = V, coords = cs}
  where cs = coords t

replaceRow :: Grid -> Row -> Row -> Grid
replaceRow g oldRow row
  | pos == -1 = error ("Couldn't find row " ++ (show oldRow) ++ " in grid " ++ (show g))
  | otherwise = (take pos g) ++ [row] ++ (drop (pos+1) g)
  where pos = indexOf oldRow g

replaceTile :: Row -> Int -> Int -> Tile -> Row
replaceTile row posX posY t
  | posX == -1 = error ("Couldn't find tile in pos " ++ (show posX) ++ " in row " ++ (show row))
  | otherwise = (take posX row) ++ [(setCoords t newCoords)] ++ (drop (posX+1) row)
  where newCoords = Coords { x = posX, y = posY }

setCoords :: Tile -> Coords -> Tile
setCoords t newCoords = Tile { content = (content t), coords = newCoords}

parseContent :: Char -> TileContent
parseContent 'O' = R
parseContent '#' = C
parseContent '.' = V
parseContent unknown = error ("Could not parse char " ++ (show unknown))

contentAsChar :: TileContent -> Char
contentAsChar R = 'O'
contentAsChar C = '#'
contentAsChar V = '.'

parseRow :: String -> Int -> Row
parseRow s y = map (\p -> Tile { content = parseContent (snd p), coords = Coords { x = (fst p), y = y } }) $ listWithIndex s

parseGrid :: String -> Grid
parseGrid inputText = map (\p -> parseRow (snd p) (fst p)) $ listWithIndex $ lines inputText

computeLoad :: Grid -> Int
computeLoad g = sum $ map (\p -> (countRocks $  (g !! (snd p))) * (fst p)) $ zip [height,(height-1)..1] [0 .. (height-1)]
  where height = length g

countRocks :: Row -> Int
countRocks row = length $ filter (\c -> (content c) == R) row

answerQuestionDayFourteen :: String -> Int
answerQuestionDayFourteen inputText = computeLoad $ moveAllRocks N g
  where g = parseGrid inputText

cycleGrid :: Grid -> Grid
cycleGrid g = moveAllRocks E $ moveAllRocks S $ moveAllRocks W $ moveAllRocks N g

answerQuestionDayFourteen' :: String -> Int
answerQuestionDayFourteen' inputText = computeLoad $ last $ take 1000000000 $ iterate (\g -> cycleGrid g) grid
  where grid = parseGrid inputText

listCycles :: Grid -> Int -> [(Int, Int)]
listCycles grid n = listWithIndex $ map (\g -> computeLoad g) $ take n $ iterate (\g -> cycleGrid g) grid

filterCycleModZeroWith :: [(Int, Int)] -> Int -> [(Int, Int)]
filterCycleModZeroWith indexedResult n = filter (\p -> (== 0) $ mod n ((+1) $ fst p)) $ tail indexedResult

data CycleResult = CycleResult { value :: Int, freq :: Int, loops :: [Int]} deriving (Show, Eq)
type CycleResultMap = Map Int CycleResult

addToMap :: (Int, Int) -> CycleResultMap -> CycleResultMap
addToMap x crm =
  case maybeEntry of
    Nothing -> Map.insert v (CycleResult { value = v, freq = 1, loops = [l]}) crm
    Just entry -> Map.insert v (CycleResult { value = v, freq = (freq entry) + 1, loops = (l:(loops entry))}) crm
  where l = fst x
        v = snd x
        maybeEntry = Map.lookup v crm

initCrm :: CycleResultMap
initCrm = Map.fromList []

getMostFrequentResult :: [(Int, Int)] -> CycleResultMap -> CycleResultMap
getMostFrequentResult [] acc = acc
getMostFrequentResult (x:xs) acc = do
      let newAcc = addToMap x acc
      getMostFrequentResult xs newAcc