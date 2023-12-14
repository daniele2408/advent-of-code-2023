{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay10 where

import Data.Char
import Data.List
import CommonFuncs
import Data.Maybe
import qualified Data.Set as DS

import Debug.Trace

data Direction = N | S | E | W | NONE | START deriving (Show, Eq, Ord)
type Tile = (Direction, Direction)

instance Eq Tile where -- TODO test me
    tl1 == tl2 = (&&) (tileContainsDirection tl1 $ fst tl2) (tileContainsDirection tl1 $ snd tl2)

data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data TileCell = TileCell { tile :: Tile, coords :: Coords } deriving (Show, Eq, Ord)
type TileRow = [TileCell]
type TileGrid = [TileRow]

startFollowingPath :: TileGrid -> Int
startFollowingPath tg = div ((followTile tg randomNextTiles startTile startTile 1) + 1) 2
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
    where maxY = (length tg) - 1
          maxX = (length $ tg !! 0) - 1

getTileCell :: TileGrid -> Coords -> Maybe TileCell
getTileCell tg cs
    | (||) ((x cs) < 0) ((y cs) < 0) = Nothing
    | (||) ((x cs) > maxX) ((y cs) > maxY) = Nothing
    | areCoordsInBound tg cs = Just $  (tg !! (y cs)) !! (x cs)
    | otherwise = Nothing
    where maxY = (length tg) - 1
          maxX = (length $ tg !! 0) - 1

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
          maxY = (length tg) - 1
          maxX = (length $ tg !! 0) - 1
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
          isNextPosCorrect = isDirectionContainedBy maybeDirection t2
          amIOpenToThat = amIOpenToDirection maybeDirection t1

isDirectionContainedBy :: Maybe Direction -> TileCell -> Bool
isDirectionContainedBy Nothing _ = False
isDirectionContainedBy (Just d) t
  | tileCellContainsDirection t START = True
  | otherwise = tileCellContainsDirection t d

amIOpenToDirection :: Maybe Direction -> TileCell -> Bool
amIOpenToDirection Nothing _ = False
amIOpenToDirection (Just d) t
  | tileCellContainsDirection t START = True
  | d == N = tileCellContainsDirection t S
  | d == S = tileCellContainsDirection t N
  | d == W = tileCellContainsDirection t E
  | d == E = tileCellContainsDirection t W

tileCellContainsDirection :: TileCell -> Direction -> Bool
tileCellContainsDirection tc d = (||) ((fst t) == d) ((snd t) == d)
  where t = tile tc

tileContainsDirection :: Tile -> Direction -> Bool
tileContainsDirection t d = (||) ((fst t) == d) ((snd t) == d)

-- which direction is tile t1 respect to t2
whichDirectionIsTile :: TileCell -> TileCell -> Maybe Direction
whichDirectionIsTile t1 t2
  | (&&) sameCol (y1 > y2) = Just S
  | (&&) sameCol (y1 < y2) = Just N
  | (&&) sameRow (x1 > x2) = Just E
  | (&&) sameRow (x1 < x2) = Just W
  | otherwise = Nothing
  where x1 = x $ coords t1
        y1 = y $ coords t1
        x2 = x $ coords t2
        y2 = y $ coords t2
        sameCol = x1 == x2
        sameRow = y1 == y2

data Side = L | R deriving (Show, Eq)

data GroundCellContainers = GroundCellContainers {
    left :: DS.Set TileCell,
    right :: DS.Set TileCell
} deriving (Show)

emptyGcc :: GroundCellContainers
emptyGcc = GroundCellContainers { left = DS.fromList [], right = DS.fromList [] }

followTileKeepPath :: TileGrid -> TileCell -> TileCell -> TileCell -> DS.Set TileCell -> DS.Set TileCell
followTileKeepPath gc currentTile previousTile target acc
    | nextTile == target = (DS.insert currentTile $ DS.insert previousTile acc)
--    | otherwise = trace ((show previousTile) ++ " --> " ++ (show currentTile) ++ " il sacco ha " ++ (show $ length acc) ++ " elementi") followTileKeepPath gc nextTile currentTile target (DS.insert previousTile acc)
    | otherwise = followTileKeepPath gc nextTile currentTile target (DS.insert previousTile acc)
    where nextTile = head $ filter (\ct -> ct /= previousTile) $ getConnectedTiles gc currentTile

startFindAreaPerimeter :: TileGrid -> Int
startFindAreaPerimeter tg = findAllNonPathTilesInArea tg path perimeter
    where startTile = findStart tg
          nextTiles = filter (\t -> areTilesConnected startTile t) $ accumulateJustsFromMaybes (map (\d -> peekDirection tg startTile d) [N, S, W, E]) []
          randomNextTile = nextTiles !! 0
          path = followTileKeepPath tg randomNextTile startTile startTile (DS.fromList [startTile])
          turnCounter = addTurnMovingFromTo startTile randomNextTile TurnCounter { rightTurns = 0, leftTurns = 0}
          perimeter = findInnerAreaPerimeterExcludePath tg randomNextTile startTile startTile path turnCounter (emptyGcc)

findAllNonPathTilesInArea :: TileGrid -> DS.Set TileCell -> DS.Set TileCell -> Int
findAllNonPathTilesInArea tg path tiles = length inAreaNotPathCoords
    where coordsTiles = DS.map (\t -> coords t) tiles
          minX = minimum $ DS.map (\c -> x c) $ coordsTiles
          maxX = maximum $ DS.map (\c -> x c) $ coordsTiles
          minY = minimum $ DS.map (\c -> y c) $ coordsTiles
          maxY = maximum $ DS.map (\c -> y c) $ coordsTiles
          mostNortWestCoords = Coords { x = minX, y = minY }
          mostNortEastCoords = Coords { x = maxX, y = minY }
          mostSouthWestCoords = Coords { x = minX, y = maxY }
          mostSouthEastCoords = Coords { x = maxX, y = maxY }
          coordsGrid = concat $ map (\l -> map (\t -> coords t) l) tg
          coordsPath = DS.map (\x -> coords x) path
          inAreaCoords = DS.fromList $ filter (\c -> isInArea c minX maxX minY maxY) coordsGrid
          inAreaNotPathCoords = DS.filter (\c -> not $ DS.member c coordsPath) inAreaCoords

isInArea :: Coords -> Int -> Int -> Int -> Int -> Bool
isInArea cs minX maxX minY maxY = (&&) ((&&) (cx <= maxX) (cy <= maxY)) ((&&) (cx >= minX) (cy >= minY))
    where cx = x cs
          cy = y cs

answerQuestionDayTen' :: String -> Int
answerQuestionDayTen' inputText = startFindAreaPerimeter $ parseGridFromInputText inputText

findInnerAreaPerimeterExcludePath :: TileGrid -> TileCell -> TileCell -> TileCell -> DS.Set TileCell -> TurnCounter -> GroundCellContainers -> DS.Set TileCell
findInnerAreaPerimeterExcludePath gc currentTile previousTile target path turnCounter gcc
    | nextTile == target = extractInnerSet newTurnCounter gcc
--    | otherwise = trace ("\n\n############\n" ++ " previous: " ++ (show previousTile) ++ " current: " ++ (show currentTile) ++ " ----> " ++ (show nextTile) ++ "\ngcc " ++ (show newGcc) ++ "\ntCounter " ++ (show newTurnCounter)) findInnerAreaPerimeterExcludePath gc nextTile currentTile target path newTurnCounter newGcc
    | otherwise = findInnerAreaPerimeterExcludePath gc nextTile currentTile target path newTurnCounter newGcc
    where connTiles = filter (\ct -> ct /= previousTile) $ getConnectedTiles gc currentTile
          nextTile = head $ connTiles
          neighbourCellsContainer = collectNonPathTiles gc currentTile path
          newGcc = selectGroundTiles currentTile nextTile neighbourCellsContainer gcc
          newTurnCounter = addTurnMovingFromTo currentTile nextTile turnCounter

findInnerAreaPerimeter :: TileGrid -> TileCell -> TileCell -> TileCell -> TurnCounter -> GroundCellContainers -> DS.Set TileCell
findInnerAreaPerimeter gc currentTile previousTile target turnCounter gcc
    | nextTile == target = extractInnerSet newTurnCounter gcc
--    | otherwise = trace ("\n\n############\n" ++ " previous: " ++ (show previousTile) ++ " current: " ++ (show currentTile) ++ " ----> " ++ (show nextTile) ++ "\ngcc " ++ (show newGcc) ++ "\ntCounter " ++ (show newTurnCounter)) findInnerAreaPerimeter gc nextTile currentTile target newTurnCounter newGcc
    | otherwise = findInnerAreaPerimeter gc nextTile currentTile target newTurnCounter newGcc
    where connTiles = filter (\ct -> ct /= previousTile) $ getConnectedTiles gc currentTile
          nextTile = head $ connTiles
          neighbourCellsContainer = collectNonPathTiles gc currentTile (DS.fromList [])
          newGcc = selectGroundTiles currentTile nextTile neighbourCellsContainer gcc
          newTurnCounter = addTurnMovingFromTo currentTile nextTile turnCounter

extractInnerSet :: TurnCounter -> GroundCellContainers -> DS.Set TileCell
extractInnerSet tCounter gcc
  | isClockWise tCounter = trace ((show tCounter) ++ " Is clockwise, getting right! ") right gcc
  | otherwise = trace ((show tCounter) ++ " Is anti-clockwise, getting left! ") left gcc

addCellsToGroundContainer :: [TileCell] -> Side -> GroundCellContainers -> GroundCellContainers
addCellsToGroundContainer ts s gcc = foldl (\acc t -> (addToGroundContainer t s acc)) gcc ts

addToGroundContainer :: TileCell -> Side -> GroundCellContainers -> GroundCellContainers
addToGroundContainer t s gcc
    | s == L = GroundCellContainers { left = (DS.insert t (left gcc)), right = (right gcc) }
    | s == R = GroundCellContainers { left = (left gcc), right = (DS.insert t (right gcc)) }

getTileCellIfGround :: TileGrid -> TileCell -> Direction -> Maybe TileCell
getTileCellIfGround tg tc d
    | maybeNextTileCell == Nothing = Nothing
    | isTileCellGround maybeNextTileCell = maybeNextTileCell
    where maybeNextTileCell = getTileCell tg (generateCoords (coords tc) d)

isTileCellGround :: Maybe TileCell -> Bool
isTileCellGround Nothing = False
isTileCellGround (Just tc) = (fst $ tile tc) == NONE

keepIfTileCellGround :: Maybe TileCell -> Maybe TileCell
keepIfTileCellGround maybeTc
  | isTileCellGround maybeTc = maybeTc
  | otherwise = Nothing

ifInSetDontGoThrough :: (Ord a) => DS.Set a -> Maybe a -> Maybe a
ifInSetDontGoThrough _ Nothing = Nothing
ifInSetDontGoThrough setA (Just a)
    | DS.member a setA = Nothing
    | otherwise = Just a

collectNonPathTiles :: TileGrid -> TileCell -> DS.Set TileCell -> NeighboursCellContainer
collectNonPathTiles tg tc path = NeighboursCellContainer {
            n = ifInSetDontGoThrough path $ if' (tileCellContainsDirection tc N) Nothing (getTileCell tg (generateCoords ctc N)),
            s = ifInSetDontGoThrough path $ if' (tileCellContainsDirection tc S) Nothing (getTileCell tg (generateCoords ctc S)),
            w = ifInSetDontGoThrough path $ if' (tileCellContainsDirection tc W) Nothing (getTileCell tg (generateCoords ctc W)),
            e = ifInSetDontGoThrough path $ if' (tileCellContainsDirection tc E) Nothing (getTileCell tg (generateCoords ctc E)),
            ne = ifInSetDontGoThrough path $ getNorthEastCell tg (Just tc),
            nw = ifInSetDontGoThrough path $ getNorthWestCell tg (Just tc),
            se = ifInSetDontGoThrough path $ getSouthEastCell tg (Just tc),
            sw = ifInSetDontGoThrough path $ getSouthWestCell tg (Just tc)
          }
          where ctc = coords tc

data TurnCounter = TurnCounter {
  rightTurns :: Int,
  leftTurns :: Int
} deriving (Show, Eq)

addTurnMovingFromTo :: TileCell -> TileCell -> TurnCounter -> TurnCounter
addTurnMovingFromTo tileCell nextTileCell tCount
  | (&&) (t == (N,W)) (maybeD == Just N) = addTurn L tCount
  | (&&) (t == (N,W)) (maybeD == Just W) = addTurn R tCount
  | (&&) (t == (N,E)) (maybeD == Just N) = addTurn R tCount
  | (&&) (t == (N,E)) (maybeD == Just E) = addTurn L tCount
  | (&&) (t == (S,E)) (maybeD == Just E) = addTurn R tCount
  | (&&) (t == (S,E)) (maybeD == Just S) = addTurn L tCount
  | (&&) (t == (S,W)) (maybeD == Just S) = addTurn R tCount
  | (&&) (t == (S,W)) (maybeD == Just W) = addTurn L tCount
  | otherwise = tCount
  where t = tile tileCell
        maybeD = whichDirectionIsTile nextTileCell tileCell


addTurn :: Side -> TurnCounter -> TurnCounter
addTurn s tc
  | s == L = TurnCounter { rightTurns = (rightTurns tc), leftTurns = ((+1) $ leftTurns tc)}
  | s == R = TurnCounter { rightTurns = ((+1) $ rightTurns tc), leftTurns = (leftTurns tc)}

isClockWise :: TurnCounter -> Bool
isClockWise tc = (rightTurns tc) > (leftTurns tc)

selectGroundTiles :: TileCell -> TileCell -> NeighboursCellContainer -> GroundCellContainers -> GroundCellContainers
selectGroundTiles tc tcNext ncc gcc
    | t == (N,S) = distributeCells tc tcNext [(nw ncc), (w ncc), (sw ncc)] [(e ncc), (ne ncc), (se ncc)] gcc
    | t == (E,W) = distributeCells tc tcNext [(n ncc), (ne ncc), (nw ncc)] [(s ncc), (se ncc), (sw ncc)] gcc
    | t == (N,W) = distributeCells tc tcNext [(s ncc), (e ncc), (sw ncc), (ne ncc), (se ncc)] [(nw ncc)] gcc
    | t == (N,E) = distributeCells tc tcNext [(s ncc), (e ncc), (sw ncc), (nw ncc), (se ncc)] [(ne ncc)] gcc
    | t == (S,E) = distributeCells tc tcNext [(n ncc), (w ncc), (sw ncc), (nw ncc), (ne ncc)] [(se ncc)] gcc
    | t == (S,W) = distributeCells tc tcNext [(n ncc), (e ncc), (se ncc), (nw ncc), (ne ncc)] [(sw ncc)] gcc
    where t = tile tc

distributeCells :: TileCell -> TileCell -> [Maybe TileCell] -> [Maybe TileCell] -> GroundCellContainers -> GroundCellContainers
distributeCells tc tcNext groupMaybeA groupMaybeB gcc
  | d == Nothing = error "Couldn't assess where next tile is!"
--  | otherwise = trace ("Current tile " ++ (show tc) ++ " è a " ++ (show d) ++ " della nextTile " ++ (show tcNext) ++ " GROUP A: " ++ (show groupA) ++ " GROUP B: " ++ (show groupB)) handleDirection (tile tc) d groupA groupB gcc
  | otherwise = handleDirection (tile tc) d groupA groupB gcc
  where d = whichDirectionIsTile tc tcNext
        groupA = accumulateJustsFromMaybes  groupMaybeA []
        groupB = accumulateJustsFromMaybes  groupMaybeB []

handleDirection :: Tile -> Maybe Direction -> [TileCell] -> [TileCell] -> GroundCellContainers -> GroundCellContainers
handleDirection _ Nothing _ _ gcc = gcc
handleDirection t (Just d) groupA groupB gcc
  | (&&) (t == (N,S)) (d == S) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (N,S)) (d == N) = addCellsToBothParts groupA R groupB L gcc
  | (&&) (t == (E,W)) (d == E) = addCellsToBothParts groupA R groupB L gcc
  | (&&) (t == (E,W)) (d == W) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (N,W)) (d == E) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (N,W)) (d == S) = addCellsToBothParts groupA R groupB L gcc
  | (&&) (t == (N,E)) (d == S) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (N,E)) (d == W) = addCellsToBothParts groupA R groupB L gcc
  | (&&) (t == (S,E)) (d == W) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (S,E)) (d == N) = addCellsToBothParts groupA R groupB L gcc
  | (&&) (t == (S,W)) (d == N) = addCellsToBothParts groupA L groupB R gcc
  | (&&) (t == (S,W)) (d == E) = addCellsToBothParts groupA R groupB L gcc
  | otherwise = error ("Couldn't find match for t " ++ (show t) ++ " and d " ++ (show d) ++ ". Direction d is supposed to be in tile t.")


addCellsToBothParts :: [TileCell] -> Side -> [TileCell] -> Side -> GroundCellContainers -> GroundCellContainers
addCellsToBothParts groupA s groupB s' gcc = addCellsToGroundContainer groupB s' $ addCellsToGroundContainer groupA s gcc
--addCellsToBothParts groupA s groupB s' gcc = trace ("Aggiungo " ++ (show $ length groupA) ++ " a " ++ (show s) ++ " e " ++ (show $ length groupB) ++ " a " ++ (show s')) addCellsToGroundContainer groupB s' $ addCellsToGroundContainer groupA s gcc

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

data NeighboursCellContainer = NeighboursCellContainer {
  n :: Maybe TileCell,
  s :: Maybe TileCell,
  w :: Maybe TileCell,
  e :: Maybe TileCell,
  ne :: Maybe TileCell,
  nw :: Maybe TileCell,
  se :: Maybe TileCell,
  sw :: Maybe TileCell
} deriving (Show)

getDiagonalTiles :: TileGrid -> TileCell -> [Maybe TileCell]
getDiagonalTiles tg tc = [nw, ne, sw, se]
    where nw = getNorthWestCell tg (Just tc)
          ne = getNorthEastCell tg (Just tc)
          sw = getSouthWestCell tg (Just tc)
          se = getSouthEastCell tg (Just tc)

generateCoords :: Coords -> Direction -> Coords
generateCoords c d
  | d == N = Coords { x = cx, y = (cy - 1) }
  | d == S = Coords { x = cx, y = (cy + 1) }
  | d == E = Coords { x = (cx + 1), y = cy }
  | d == W = Coords { x = (cx - 1), y = cy }
  | otherwise = c
  where cx = x c
        cy = y c

getNorthCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getNorthCell _ Nothing = Nothing
getNorthCell tg (Just t) = getTileCell tg (generateCoords tileCoords N)
  where tileCoords = coords t

getSouthCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getSouthCell _ Nothing = Nothing
getSouthCell tg (Just t) = getTileCell tg (generateCoords tileCoords S)
  where tileCoords = coords t

getWestCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getWestCell _ Nothing = Nothing
getWestCell tg (Just t) = getTileCell tg (generateCoords tileCoords W)
 where tileCoords = coords t

getEastCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getEastCell _ Nothing = Nothing
getEastCell tg (Just t) = getTileCell tg (generateCoords tileCoords E)
 where tileCoords = coords t

getNorthWestCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getNorthWestCell _ Nothing = Nothing
getNorthWestCell tg tc = getWestCell tg $ getNorthCell tg tc

getNorthEastCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getNorthEastCell _ Nothing = Nothing
getNorthEastCell tg tc = getEastCell tg $ getNorthCell tg tc

getSouthWestCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getSouthWestCell _ Nothing = Nothing
getSouthWestCell tg tc = getWestCell tg $ getSouthCell tg tc

getSouthEastCell :: TileGrid -> Maybe TileCell -> Maybe TileCell
getSouthEastCell _ Nothing = Nothing
getSouthEastCell tg tc = getEastCell tg $ getSouthCell tg tc

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
answerQuestionDayTen inputText = startFollowingPath $ parseGridFromInputText inputText