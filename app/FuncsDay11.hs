module FuncsDay11 where

import CommonFuncs
import qualified Data.Set as DS

data Content = Galaxy | Void deriving (Show, Eq)
data Coords = Coords { x :: Int, y :: Int } deriving (Show, Eq, Ord)
data Cell = Cell { coords :: Coords, value :: Content } deriving (Show, Eq)

type CellStripe = [Cell]
type Universe = [CellStripe]

fiatLux :: String -> Universe
fiatLux inputText = map (\p -> fiatCellStripe (snd p) (fst p)) $ listWithIndex ls
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
joinStrings (x:xs) c acc = joinStrings xs c (x ++ [c] ++ acc)

defiatUniverse :: Universe -> [String]
defiatUniverse u = map (\stripe -> cellStripeAsString stripe) u

iterativeInsertIntPositions :: [Int] -> a -> [a] -> [a]
iterativeInsertIntPositions [] x xs = xs
iterativeInsertIntPositions (i:is) x xs = iterativeInsertIntPositions is x newXs
    where newXs = insertIntoPosition i x xs

enlargeYourUniverse :: Universe -> [Int] -> Universe
enlargeYourUniverse u xs = map (\p -> fiatCellStripe (snd p) (fst p)) $ listWithIndex stripesAsStr
    where stripesAsStr = map (\stripe -> iterativeInsertIntPositions xs (deFiatContent Void) (cellStripeAsString stripe)) u