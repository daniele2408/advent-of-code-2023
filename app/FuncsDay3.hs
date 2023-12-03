module FuncsDay3 where

import CommonFuncs
import Data.List.Split
import Data.Char

data Point = Point {
  x :: Int,
  y :: Int
} deriving (Show, Eq)

type ItemCoords = (Point, Point)

addY :: Int -> Point -> Point
addY n p = Point (x p) ((y p)+n)

addX :: Int -> Point -> Point
addX n p = Point ((x p)+n) (y p)

type SchemaLine = (Int, String)
type Schema = [SchemaLine]

data SchemaItem = NumberItem { numberValue :: Int, coords :: ItemCoords }
                | SymbolItem { symbolValue :: String, coords :: ItemCoords } deriving (Show, Eq)

data Grid = Grid {
  width :: Int,
  height :: Int,
  items :: [SchemaItem]
} deriving (Show)

-- TODO questi si possono sintetizzare con list comprehension
isPointNextTo :: Point -> Point -> Bool
isPointNextTo p p'
  | (&&) ((abs $ (x p) - (x p')) <= 1) ((abs $ (y p) - (y p')) <= 1)  = True
  | otherwise = False

isCoordsNextTo :: ItemCoords -> ItemCoords -> Bool
isCoordsNextTo ic ic'
  | (fst ic) `isPointNextTo` (fst ic') = True
  | (fst ic) `isPointNextTo` (snd ic') = True
  | (snd ic) `isPointNextTo` (fst ic') = True
  | (snd ic) `isPointNextTo` (snd ic') = True
  | otherwise = False

-- TODO not cool, ci sarà un modo migliore per fare questo
isItemNextTo :: SchemaItem -> SchemaItem -> Bool
isItemNextTo si si'
  | (&&) (isNumberItem si) (isNumberItem si) = (coords si) `isCoordsNextTo` (coords si')
  | (&&) (isNumberItem si)  (not $ isNumberItem si) = (coords si) `isCoordsNextTo` (coords si')
  | (&&) (not $ isNumberItem si)  (isNumberItem si) = (coords si) `isCoordsNextTo` (coords si')
  | (&&) (not $ isNumberItem si)  (not $ isNumberItem si) = (coords si) `isCoordsNextTo` (coords si')

filterPartNumbers :: [SchemaItem] -> [SchemaItem]
filterPartNumbers sis = filter (\si' -> si' `isPartNumber` sis) sis

isPartNumber :: SchemaItem -> [SchemaItem] -> Bool
isPartNumber si sis
  | not $ isNumberItem si = False
  | otherwise = any (\si' -> si `isItemNextTo` si') $ filter (\si' -> si' /= si) sis

isNumberItem :: SchemaItem -> Bool
isNumberItem (NumberItem _ _) = True
isNumberItem _ = False

parseInputHeight :: String -> Int
parseInputHeight inputText = length $ lines inputText

parseInputWidth :: String -> Int
parseInputWidth inputText = length $ (lines inputText) !! 0

parseGrid :: String -> Grid
parseGrid inputText = Grid (parseInputWidth inputText) (parseInputHeight inputText) []

generateSchemaFromInput :: String -> Schema
generateSchemaFromInput inputText = zip [0..] (lines inputText)

parseSchema :: Schema -> [SchemaItem]
parseSchema schema = concat $ map (\sl -> parseSchemaItemsFromSchemaLine sl) schema

parseSchemaItemsFromSchemaLine :: SchemaLine -> [SchemaItem]
parseSchemaItemsFromSchemaLine schemaLine = parseSchemaLine schemaLine [] 0

parseSchemaLine :: SchemaLine -> [SchemaItem] -> Int -> [SchemaItem]
parseSchemaLine (nLine, (x:xs)) acc pos
  | (&&) (xs==[]) (x=='.') = acc
  | (x:xs) == [] = acc
  | x == '.' = parseSchemaLine (nLine, xs) acc (pos+1)
  | not $ isDigit x = do
    let symbol = takeWhileIsDigit (x:xs)
    let lenSymbol = length symbol
    parseSchemaLine (nLine, xs) ((SymbolItem [x] (generateCoords nLine pos 0)):acc) (pos+1)
  | otherwise = do
     let symbol = takeWhileIsDigit (x:xs)
     let lenSymbol = length symbol
     let newSchemaLine = (nLine, (drop lenSymbol (x:xs)))
     let newAcc = ((parseSchemaItem symbol (nLine) pos):acc)
     let newPos = pos + 1
     parseSchemaLine newSchemaLine newAcc (newPos+lenSymbol-1)
parseSchemaLine (_, []) acc pos = acc

parseSchemaItem :: String -> Int -> Int -> SchemaItem
parseSchemaItem s x y
  | isStringAllDigits s = NumberItem (convertStrToInt s) (generateCoords x y ((length s)-1))
  | otherwise = error "parseSchemaItem error - This shouldn't be anything but NumberItem"
--  | otherwise = SymbolItem s (generateCoords x y ((length s)-1)) -- TODO can I put this into where clause?

generateCoords :: Int -> Int -> Int -> ItemCoords
generateCoords x y n = do
    let startPoint = Point x y
    let endPoint = (addY n startPoint)
    (startPoint, endPoint)

takeWhileNotADot :: String -> String
takeWhileNotADot s = takeWhile (\x -> x /= '.') s

takeWhileIsDigit :: String -> String
takeWhileIsDigit s = takeWhile (\x -> isDigit x) s

answerQuestionDayThree :: String -> Int
answerQuestionDayThree inputText = sum $ map (\si -> (numberValue si)) $ filterPartNumbers $ parseSchema $ generateSchemaFromInput inputText