module FuncsDay11 where

import CommonFuncs
import Data.List
import qualified Data.Set as DS
import Debug.Trace


data Content = Galaxy | Void deriving (Show, Eq)
data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data Cell = Cell { coords :: Coords, value :: Content } deriving (Show, Eq)

type CellStripe = [Cell]
type Universe = [CellStripe]
type CellCatalog = [(Int, Cell)]

answerQuestionDayEleven :: String -> Int
answerQuestionDayEleven inputText = sum $ map (\p -> countDistance universe (fst p) (snd p)) pairs
    where universe = expandUniverse $ fiatLux inputText
          galaxies = map (\p -> (snd p)) $ extractGalaxyCatalog universe
          pairs = generateUniquePairs galaxies []

fiatLux :: String -> Universe
fiatLux inputText = map (\p -> fiatCellStripe (snd p) (fst p)) $ filter (\l -> (snd l) /= "") $ listWithIndex ls
    where ls = lines inputText

fiatCellStripe :: String -> Int -> CellStripe
fiatCellStripe l y = map (\p -> fiatCell (snd p) (Coords { x = (fst p), y = y })) $ listWithIndex l

listWithIndex :: [a] -> [(Int, a)]
listWithIndex xs = zip [0..((length xs)-1)] xs

fiatCell :: Char -> Coords -> Cell
fiatCell s c
    | s == '#' = Cell { coords = c , value = Galaxy }
    | s == '.' = Cell { coords = c , value = Void }

deFiatContent :: Content -> Char
deFiatContent c
    | c == Galaxy = '#'
    | c == Void = '.'

findVoidStripesPosition :: Universe -> [Int]
findVoidStripesPosition u = map (\p -> fst p) $ filter (\p -> isStripeVoid $ snd p) $ listWithIndex u

findVoidColumnPosition :: Universe -> [Int]
findVoidColumnPosition u = filter (\i -> not $ DS.member i positionsNonVoid) [0..maxWidth]
    where positionsNonVoid = DS.fromList $ concat $ map (\stripe -> collectNonVoidPosition stripe) u
          maxWidth = (length $ u !! 0) - 1

collectNonVoidPosition :: CellStripe -> [Int]
collectNonVoidPosition cs = map (\p -> fst p) $ filter (\p -> not $ (==Void) $ value $ snd p) $ listWithIndex cs

isStripeVoid :: CellStripe -> Bool
isStripeVoid cs = all (\c -> (==Void) $ value c) cs

insertIntoPosition :: Int -> a -> [a] -> [a]
insertIntoPosition pos x xs
    | pos < 0 = error "Position can't be negative"
    | pos > (length xs) = error "Position can't be out of bound actual list"
    | otherwise = (take (pos) xs) ++ [x] ++ (drop (pos) xs)

cellStripeAsString :: CellStripe -> String
cellStripeAsString cs = map (\el -> deFiatContent $ value el) cs

expandUniverse :: Universe -> Universe
expandUniverse u = deeperUniverse
    where voidRowPos = findVoidStripesPosition u
          voidColPos = findVoidColumnPosition u
          largerUniverse = enlargeYourUniverse u voidColPos
          newMaxWidth = length $ largerUniverse !! 0
          newVoidStripe = take newMaxWidth $ repeat (deFiatContent Void)
          deeperUniverse = fiatLux $ joinStrings (iterativeInsertIntPositions voidRowPos newVoidStripe $ defiatUniverse largerUniverse) '\n' ""

joinStrings :: [String] -> Char -> String -> String
joinStrings [] c acc = acc
joinStrings (x:xs) c acc = joinStrings xs c (acc ++ x ++ [c])

defiatUniverse :: Universe -> [String]
defiatUniverse u = map (\stripe -> cellStripeAsString stripe) u

iterativeInsertIntPositions :: [Int] -> a -> [a] -> [a]
iterativeInsertIntPositions [] x xs = xs
iterativeInsertIntPositions poss x xs = iterativeInsertIntPositions tailPoss x newXs
    where (maxPos:tailPoss) = reverse $ sort $ poss -- important, or it will mess up the consecutive inserts
          newXs = insertIntoPosition maxPos x xs

enlargeYourUniverse :: Universe -> [Int] -> Universe
enlargeYourUniverse u xs = map (\p -> fiatCellStripe (snd p) (fst p)) $ listWithIndex stripesAsStr
    where stripesAsStr = map (\stripe -> iterativeInsertIntPositions xs (deFiatContent Void) (cellStripeAsString stripe)) u

isGalaxy :: Cell -> Bool
isGalaxy c = (value c) == Galaxy

extractGalaxyCatalog :: Universe -> CellCatalog
extractGalaxyCatalog u = listWithIndex $ filter (\c -> isGalaxy c) $ concat u

generateUniquePairs :: [a] -> [(a, a)] -> [(a, a)]
generateUniquePairs [] acc = acc
generateUniquePairs (x:xs) acc = generateUniquePairs xs (combos ++ acc)
  where combos = map (\e -> (x,e)) xs

getCell :: Universe -> Coords -> Maybe Cell
getCell u cs
    | (||) (cx < 0) (cx > maxX) = Nothing
    | (||) (cy < 0) (cy > maxY) = Nothing
    | otherwise = Just ((u !! cy) !! cx)
    where cx = x cs
          cy = y cs
          maxX = (length $ u !! 0) - 1
          maxY = (length u) - 1

countDistance :: Universe -> Cell -> Cell -> Int
countDistance u cellStart cellTarget = countDistanceIterative u cellStart cellStart cellTarget 0

countDistanceIterative :: Universe -> Cell -> Cell -> Cell -> Int -> Int
countDistanceIterative u previousCell currentCell target acc
  | closestNeighbourToTarget == target = trace ("Bingo!: " ++ (show $ acc + 1)) acc+1
  | otherwise = countDistanceIterative u currentCell closestNeighbourToTarget target (acc+1)
     where closestNeighbourToTarget = snd $ head $ filter (\p -> (snd p) /= previousCell) $ orderNeighboursByDistanceAsc u currentCell target

getX :: Cell -> Int
getX c = x $ coords c

getY :: Cell -> Int
getY c = y $ coords c

getNeighbours :: Universe -> Cell -> [Cell]
getNeighbours universe cell = accumulateJustsFromMaybes [u, d, l, r] []
    where u = goUp universe cell
          d = goDown universe cell
          l = goLeft universe cell
          r = goRight universe cell

orderNeighboursByDistanceAsc :: Universe -> Cell -> Cell -> [(Int, Cell)]
orderNeighboursByDistanceAsc u cell target = sortBy sortPairsByFirst $ map (\n -> ((getDistance target n), n)) neighbours
    where neighbours = getNeighbours u cell

-- is cell' down respect to cell?
isDown :: Cell -> Cell -> Bool
isDown cell cell' = (getY cell) < (getY cell')

isUp :: Cell -> Cell -> Bool
isUp cell cell' = not $ isDown cell cell'

isRight :: Cell -> Cell -> Bool
isRight cell cell' = (getX cell) < (getX cell')

isLeft :: Cell -> Cell -> Bool
isLeft cell cell' = not $ isRight cell cell'

goDown :: Universe -> Cell -> Maybe Cell
goDown u c = getCell u Coords { x = (getX c) , y = ((+ 1) $ getY c)}

goUp :: Universe -> Cell -> Maybe Cell
goUp u c = getCell u Coords { x = (getX c) , y = ((getY c) - 1)}

goLeft :: Universe -> Cell -> Maybe Cell
goLeft u c = getCell u Coords { x = ((getX c) - 1) , y = (getY c)}

goRight :: Universe -> Cell -> Maybe Cell
goRight u c = getCell u Coords { x = ((getX c) + 1) , y = (getY c)}

getDistanceX :: Cell -> Cell -> Int
getDistanceX cell cell' = (abs $ (x cs) - (x cs'))
    where cs = coords cell
          cs' = coords cell'

getDistanceY :: Cell -> Cell -> Int
getDistanceY cell cell' = (abs $ (y cs) - (y cs'))
    where cs = coords cell
          cs' = coords cell'

getDistance :: Cell -> Cell -> Int
getDistance cell cell' = (getDistanceX cell cell') + (getDistanceY cell cell')
