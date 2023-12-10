{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay10 where

import Data.Char
import Data.List
import CommonFuncs
import Data.Maybe

data Direction = N | S | E | W | NONE | START deriving (Show, Eq)
type Tile = (Direction, Direction)

instance Eq Tile where -- TODO test me
    tl1 == tl2 = not $ (&&) ((fst tl1) /= (fst tl2)) ((fst tl1) /= (snd tl2))

data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq)
data TileCell = TileCell { tile :: Tile, coords :: Coords } deriving (Show, Eq)
type TileRow = [TileCell]
type TileGrid = [TileRow]

getHalfLoopStep :: TileGrid -> Int
getHalfLoopStep tg = div ((followTile tg randomNextTiles startTile 1) + 1) 2
    where startTile = findStart tg
          nextTiles = filter (\t -> (&&) ((fst $ tile t) /= START) ((fst $ tile t) /= NONE)) $ filterMaybes (map (\d -> peekDirection tg startTile d) [N, S, W, E]) []
          randomNextTiles = nextTiles !! 0

findStart :: TileGrid -> TileCell
findStart tg = head $ take 1 $ filter (\tc -> (fst $ tile tc) == START) $ concat tg

followTile :: TileGrid -> TileCell -> TileCell -> Int -> Int
followTile gc currentTile previousTile steps
    | (fst tileType) == START = steps
    | otherwise = followTile gc nextTile currentTile (steps+1)
    where tileType = tile currentTile
          nextTile = head $ filter (\ct -> ct /= previousTile) $ getConnectedTiles gc currentTile

areCoordsInBound :: TileGrid -> Coords -> Bool
areCoordsInBound tg cs = (&&) (maxX >= (x cs)) (maxY >= (y cs))
    where maxX = length tg
          maxY = length $ tg !! 0

getTileCell :: TileGrid -> Coords -> Maybe TileCell
getTileCell tg cs
    | areCoordsInBound tg cs = Just $  (tg !! (y cs)) !! (x cs)
    | otherwise = Nothing
    where maxX = length tg
          maxY = length $ tg !! 0

peekDirection :: TileGrid -> TileCell -> Direction -> Maybe TileCell
peekDirection tg tc d
    | d == N = getTileCell tg Coords { x = cx, y = (cy - 1) }
    | d == S = getTileCell tg Coords { x = cx, y = (cy + 1) }
    | d == W = getTileCell tg Coords { x = (cx - 1), y = cy }
    | d == E = getTileCell tg Coords { x = (cx + 1), y = cy }
    | otherwise = Nothing
    where cx = x $ coords tc
          cy = y $ coords tc

filterMaybes :: [Maybe a] -> [a] -> [a]
filterMaybes [] acc = acc
filterMaybes ((Just x):xs) acc = filterMaybes xs (x:acc)
filterMaybes (Nothing:xs) acc = filterMaybes xs acc

getConnectedTiles :: TileGrid -> TileCell -> [TileCell]
getConnectedTiles tg tl = filter (\t -> areTilesConnected (tile tl) (tile t)) $ filterMaybes (map (\cs -> getTileCell tg cs) $ filter (\cs -> areCoordsInBound tg cs) neighboursCoords) []
    where cs = coords tl
          csX = x cs
          csY = y cs
          maxX = length tg
          maxY = (length $ tg !! 0) - 1
          startX = max 0 (csX-1)
          endX = min (csX+1) maxX
          startY = max 0 (csY-1)
          endY = min (csY+1) maxY
          horizontalRange = [startX .. endX]
          verticalRange = [startY .. endY]
          neighboursCoords = map (\p -> Coords { x = (fst p), y = (snd p)}) $ filter (\p -> (&&) ((fst p) /= csX) ((snd p) /= csY) ) $ zip horizontalRange verticalRange

areTilesConnected :: Tile -> Tile -> Bool
areTilesConnected t1 t2
    | (||) (t1dirA == t2dirB) (t1dirA == t2dirB) = True
    | (||) (t2dirA == t2dirB) (t2dirA == t2dirB) = True
    | otherwise = False
    where t1dirA = fst t1
          t1dirB = snd t1
          t2dirA = fst t2
          t2dirB = snd t2

parseTileFromChar :: Char -> Int -> Int -> TileCell
parseTileFromChar c y x
    | c == '|' = TileCell { tile = (N, S), coords = coords }
    | c == '-' = TileCell { tile = (E, W), coords = coords }
    | c == 'L' = TileCell { tile = (N, E), coords = coords }
    | c == 'J' = TileCell { tile = (N, W), coords = coords }
    | c == '7' = TileCell { tile = (S, W), coords = coords }
    | c == 'F' = TileCell { tile = (S, E), coords = coords }
    | c == '.' = TileCell { tile = (NONE, NONE), coords = coords }
    | c == 'S' = TileCell { tile = (START, START), coords = coords }
    | otherwise = error ("Character " ++ [c] ++ " not recognized as valid to parse.")
    where coords = Coords { x = x, y = y }

parseTileRowFromLine :: String -> Int -> TileRow
parseTileRowFromLine l y = map (\p -> parseTileFromChar (snd p) y (fst p)) $ zip [0..((length l)-1)] l

parseGridFromInputText :: String -> TileGrid
parseGridFromInputText inputText = map (\p -> parseTileRowFromLine (snd p) (fst p)) $ zip [0..((length ls)-1)] ls
    where ls = lines inputText

answerQuestionDayTen :: String -> Int
answerQuestionDayTen inputText = getHalfLoopStep $ parseGridFromInputText inputText