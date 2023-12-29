{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay13 where

import CommonFuncs
import Data.List
import Data.Char
import Data.Either
import Data.Maybe
import Data.List.Split

import qualified Data.Set as DS

type Row = [Char]
type Grid = [Row]
type ReflectionPos = (Int, Int)

data ReflectionData = ReflectionData { posH :: Int, posV :: Int} deriving (Show, Eq)

maxWidth :: Grid -> Int
maxWidth g = length $ g !! 0

maxHeight :: Grid -> Int
maxHeight g = length $ g

findAllSameRowPos :: Grid -> [Int]
findAllSameRowPos g = map (\p -> fst p) $ filter (\p -> (fst (snd p)) == (snd (snd p))) $ listWithIndex $ map (\p -> ((g !! (fst p)), g !! (snd p))) $ pairwise [0 .. ((maxHeight g) - 1)]

generateGrids :: String -> [Grid]
generateGrids inputText = map (\chunk -> generateGrid chunk) $ splitOn "\n\n" inputText

generateGrid :: String -> Grid
generateGrid inputText = splitOn "\n" inputText

isValidHorizReflectionPos :: Grid -> Int -> Bool
isValidHorizReflectionPos g pos = all (\p -> (fst p) == (snd p)) $ map (\p -> (((g !!) $ fst p), ((g !!) $ snd p))) $ listPairWiseOutwardPositions g pos

getHorizReflectionPos :: Grid -> Maybe Int
getHorizReflectionPos g
    | horizReflPos == [] = Nothing
    | otherwise = Just $ head horizReflPos
    where horizReflPos = filter (\r -> isValidHorizReflectionPos g r) $ findAllSameRowPos g

getVertReflectionPos :: Grid -> Maybe Int
getVertReflectionPos g = getHorizReflectionPos $ transpose g

answerQuestionDayThirteen :: String -> Int
answerQuestionDayThirteen inputText = sum $ map (\g -> computeScore $ parseGrid g) $ generateGrids inputText

answerQuestionDayThirteen' :: String -> Int
answerQuestionDayThirteen' inputText = sum $ map (\g -> removeSmudgesFromGrid g) $ generateGrids inputText

parseGrid :: Grid -> ReflectionData
parseGrid g =
    case (horizPos, vertPos) of
--        (Just p, Just p') -> ReflectionData { posH = p, posV = p'}
        (Just p, Nothing) -> ReflectionData { posH = p, posV = -1}
        (Nothing, Just p) -> ReflectionData { posH = -1, posV = p}
        (_, _) -> ReflectionData { posH = -1, posV = -1}
    where horizPos = getHorizReflectionPos g
          vertPos = getVertReflectionPos g

computeScore :: ReflectionData -> Int
computeScore rd = (pH * 100) + pV
    where pH = (+ 1) $ posH rd
          pV = (+ 1) $ posV rd

--removeSmudgesFromGrid :: Grid -> Grid
--removeSmudgesFromGrid g
--    | sameRowPos == [] = g
--    | otherwise = replaceRowInGrid g (head sameRowPos)
--    where sameRowPos = findAllSameRowPos g

--removeSmudgesFromGrid :: Grid -> Grid
--removeSmudgesFromGrid g
--    | newGrid == g = handleReplaceGridRowPos (transpose g) $ findAllSameRowPos $ transpose g
--    | otherwise = newGrid
--    where newGrid = handleReplaceGridRowPos g $ findAllSameRowPos g

handleReplaceGridRowPos :: Grid -> [Int] -> Grid
handleReplaceGridRowPos g [] = g
handleReplaceGridRowPos g xs = replaceRowInGridListPos g xs

replaceRowInGridListPos :: Grid -> [Int] -> Grid
replaceRowInGridListPos g [] = g
replaceRowInGridListPos g (p:ps)
    | row == [] = replaceRowInGridListPos g ps
    | otherwise = replaceAtIndex posToReplace row g
    where (posToReplace, row) = extractDifferentRow g p

--replaceRowInGrid :: Grid -> Int -> Grid
--replaceRowInGrid g reflPos
--    | row == [] = g
--    | otherwise = replaceAtIndex posToReplace row g
--    where (posToReplace, row) = extractDifferentRow g reflPos

removeSmudgesFromGrid :: Grid -> Int
removeSmudgesFromGrid g =
    case (posRow, posCol) of
        (Just i, _) -> 100 * (i + 1)
        (_, Just i) -> i + 1
        (_, _) -> error ("Couldnt find smudge for " ++ (show g))
    where posRow = findReflectionPointAfterClean g
          posCol = findReflectionPointAfterClean $ transpose g

findReflectionPointAfterClean :: Grid -> Maybe Int
findReflectionPointAfterClean g
    | pairMidPointCleanGrid == [] = Nothing
    | otherwise = Just $ fst $ head pairMidPointCleanGrid
    where diffsByOne = findAllPairPosDiffsByOne g
          validMidPoints = accumulateJustsFromMaybes (map (\dbo -> generateMidPoint dbo) diffsByOne) []
          pairMidPointCleanGrid = filter (\p -> isValidHorizReflectionPos (snd p) (fst p)) $ map (\triple -> (t triple, copyPasteRowFromTo (s triple) (f triple) g)) validMidPoints

data Triplet = Triplet { f :: Int, s :: Int, t :: Int} deriving (Show, Eq)

copyPasteRowFromTo :: Int -> Int -> Grid -> Grid
copyPasteRowFromTo posToCopy posToPaste g = replaceAtIndex posToPaste rowToCopy g
    where rowToCopy = g !! posToCopy

generateMidPoint :: (Int, Int) -> Maybe Triplet
generateMidPoint (f,s)
    | (== 0) $ (mod (s + f - 1) 2) = Just $ Triplet { f=f, s=s , t=div (s + f - 1) 2 }
    | otherwise = Nothing

findAllPairPosDiffsByOne :: Grid -> [(Int, Int)]
findAllPairPosDiffsByOne g = filter (\p -> isDiffByOne (g !! (fst p)) (g !! (snd p))) posPairs
    where posPairs = generateUniquePairs [0 .. ((length g) - 1)] []

isDiffByOne :: Row -> Row -> Bool
isDiffByOne r1 r2 = (== 1) $ length countDiff
    where countDiff = filter (\p -> (fst $ snd p) /= (snd $ snd p)) $ listWithIndex $ zip r1 r2

findSmudge :: Row -> Row -> Maybe Int
findSmudge r1 r2
    | (== 1) $ length countDiff = Just $ fst $ head countDiff
    | otherwise = Nothing
    where countDiff = filter (\p -> (fst $ snd p) /= (snd $ snd p)) $ listWithIndex $ zip r1 r2

removeSmudge :: Row -> Row -> Row
removeSmudge r1 r2 =
    case posSmudge of
        Just p -> switchCharAtPos r1 p
        Nothing -> r1
    where posSmudge = findSmudge r1 r2

switchCharAtPos :: Row -> Int -> Row
switchCharAtPos row pos
    | c == '#' = replaceAtIndex pos '.' row
    | c == '.' = replaceAtIndex pos '#' row
    | otherwise = row
    where c = row !! pos

extractDifferentRow :: Grid -> Int -> (Int, Row)
extractDifferentRow g pos = handleDiffByOne g $ filter (\p -> isDiffByOne (fst p) (snd p)) $ map (\p -> (((g !!) $ fst p), ((g !!) $ snd p))) $ listPairWiseOutwardPositions g pos

handleDiffByOne :: Grid -> [(Row, Row)] -> (Int, Row)
handleDiffByOne _ [] = (-1, [])
handleDiffByOne g (x:_) = ((indexOf (fst x) g), (removeSmudge (fst x) (snd x)))