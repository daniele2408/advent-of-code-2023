{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay10 where

import Data.Char
import Data.List
import CommonFuncs
import Data.Maybe
import qualified Data.Set as Set

import Debug.Trace

data Direction = N | S | E | W | NONE | START deriving (Show, Eq)
type Tile = (Direction, Direction)

instance Eq Tile where -- TODO test me
    tl1 == tl2 = not $ (&&) ((fst tl1) /= (fst tl2)) ((fst tl1) /= (snd tl2))

data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq)
data TileCell = TileCell { tile :: Tile, coords :: Coords } deriving (Show, Eq)
type TileRow = [TileCell]
type TileGrid = [TileRow]

getHalfLoopStep :: TileGrid -> Int
getHalfLoopStep tg = div ((followTile tg randomNextTiles startTile startTile 1) + 1) 2
    where startTile = findStart tg
          nextTiles = filter (\t -> areTilesConnected startTile t) $ accumulateJustsFromMaybes (map (\d -> peekDirection tg startTile d) [N, S, W, E]) []
          randomNextTiles = nextTiles !! 0

findStart :: TileGrid -> TileCell
findStart tg = head $ take 1 $ filter (\tc -> (fst $ tile tc) == START) $ concat tg

followTile :: TileGrid -> TileCell -> TileCell -> TileCell -> Int -> Int
followTile gc currentTile previousTile target steps
    | nextTile == target = steps
    | otherwise = followTile gc nextTile currentTile target (steps+1)
    where nextTile = head $ filter (\ct -> ct /= previousTile) $ getConnectedTiles gc currentTile

areCoordsInBound :: TileGrid -> Coords -> Bool
areCoordsInBound tg cs = (&&) (maxX >= (x cs)) (maxY >= (y cs))
    where maxX = (length tg) - 1
          maxY = (length $ tg !! 0) - 1

getTileCell :: TileGrid -> Coords -> Maybe TileCell
getTileCell tg cs
    | (||) ((x cs) < 0) ((y cs) < 0) = Nothing
    | (||) ((x cs) > maxX) ((y cs) > maxY) = Nothing
    | areCoordsInBound tg cs = Just $  (tg !! (y cs)) !! (x cs)
    | otherwise = Nothing
    where maxX = (length tg) - 1
          maxY = (length $ tg !! 0) - 1

peekDirection :: TileGrid -> TileCell -> Direction -> Maybe TileCell
peekDirection tg tc d
    | d == N = getTileCell tg Coords { x = cx, y = (cy - 1) }
    | d == S = getTileCell tg Coords { x = cx, y = (cy + 1) }
    | d == W = getTileCell tg Coords { x = (cx - 1), y = cy }
    | d == E = getTileCell tg Coords { x = (cx + 1), y = cy }
    | otherwise = Nothing
    where cx = x $ coords tc
          cy = y $ coords tc

accumulateJustsFromMaybes :: [Maybe a] -> [a] -> [a]
accumulateJustsFromMaybes [] acc = acc
accumulateJustsFromMaybes ((Just x):xs) acc = accumulateJustsFromMaybes xs (x:acc)
accumulateJustsFromMaybes (Nothing:xs) acc = accumulateJustsFromMaybes xs acc

getConnectedTiles :: TileGrid -> TileCell -> [TileCell]
getConnectedTiles tg tl = filter (\t -> areTilesConnected tl t) $ getNeighbourTiles tg tl

getNeighbourTiles :: TileGrid -> TileCell -> [TileCell]
getNeighbourTiles tg tl = accumulateJustsFromMaybes (map (\cs -> getTileCell tg cs) $ filter (\cs -> areCoordsInBound tg cs) neighboursCoords) []
    where cs = coords tl
          csX = x cs
          csY = y cs
          maxX = (length tg) - 1
          maxY = (length $ tg !! 0) - 1
          startX = max 0 (csX-1)
          endX = min (csX+1) maxX
          startY = max 0 (csY-1)
          endY = min (csY+1) maxY
          horizontalRange = [startX .. endX]
          verticalRange = [startY .. endY]
          neighboursCoords = map (\p -> Coords { x = (p !! 0), y = (p !! 1)}) $ filter (\p -> not $  (&&) ((p !! 0) == csX) ((p !! 1) == csY) ) $ sequence [horizontalRange,verticalRange]

areTilesConnected :: TileCell -> TileCell -> Bool
areTilesConnected t1 t2 = (&&) isNextPosCorrect amIOpenToThat
    where maybeDirection = whichDirectionIsTile t1 t2
          isNextPosCorrect = doDirectionsMatch maybeDirection t2
          amIOpenToThat = amIOpenToDirection maybeDirection t1

doDirectionsMatch :: Maybe Direction -> TileCell -> Bool
doDirectionsMatch Nothing _ = False
doDirectionsMatch (Just d) t
  | tileContainsDirection t START = True
  | otherwise = tileContainsDirection t d

amIOpenToDirection :: Maybe Direction -> TileCell -> Bool
amIOpenToDirection Nothing _ = False
amIOpenToDirection (Just d) t
  | tileContainsDirection t START = True
  | d == N = tileContainsDirection t S
  | d == S = tileContainsDirection t N
  | d == W = tileContainsDirection t E
  | d == E = tileContainsDirection t W

tileContainsDirection :: TileCell -> Direction -> Bool
tileContainsDirection tc d = (||) ((fst t) == d) ((snd t) == d)
  where t = tile tc

whichDirectionIsTile :: TileCell -> TileCell -> Maybe Direction
whichDirectionIsTile t1 t2
  | (&&) sameRow (y1 > y2) = Just S
  | (&&) sameRow (y1 < y2) = Just N
  | (&&) sameCol (x1 > x2) = Just E
  | (&&) sameCol (x1 < x2) = Just W
  | otherwise = Nothing
  where x1 = x $ coords t1
        y1 = y $ coords t1
        x2 = x $ coords t2
        y2 = y $ coords t2
        sameRow = x1 == x2
        sameCol = y1 == y2

data Side = L | R deriving (Show, Eq)

data GroundCellContainers = GroundCellContainers {
    left :: Set TileCell,
    right :: Set TileCell
}

addToGroundContainer :: TileCell -> Side -> GroundCellContainers -> GroundCellContainers
addToGroundContainer t s gcc
    | s == L = GroundCellContainers { left = (insert t (left gcc)), right = (right gcc) }
    | s == D = GroundCellContainers { left = (left gcc), right = (insert t (right gcc)) }


collectGroundTiles :: TileGrid -> TileCell -> GroundCellContainers -> GroundCellContainers
collectGroundTiles tg tc rightTcs leftTcs
    where neighbourTiles =

getNorthCell :: TileGrid -> TileCell -> Maybe TileCell
getNorthCell tg tc = getTileCell tg N

getSouthCell :: TileGrid -> TileCell -> Maybe TileCell
getSouthCell tg tc = getTileCell tg S

getWestCell :: TileGrid -> TileCell -> Maybe TileCell
getWestCell tg tc = getTileCell tg W

getEastCell :: TileGrid -> TileCell -> Maybe TileCell
getEastCell tg tc = getTileCell tg E

getNorhtWestCell :: TileGrid -> TileCell -> MaybeTileCell
getNorhtWestCell tg tc
    |
    |
    where maybeNorthCell = getNorthCell tg tc

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