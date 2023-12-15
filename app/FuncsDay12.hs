{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay12 where

import CommonFuncs
import Data.List
import qualified Data.Set as DS

data Status = B | O | U deriving (Show, Eq)
type SpringRecordNumeric = [Int]
type SpringRecordSymbols = [Status]
data SpringRecord = SpringRecord { num :: SpringRecordNumeric, sym :: SpringRecordSymbols } deriving (Show, Eq)

instance Show SpringRecordSymbols where
  show srs = foldl (\acc x -> acc ++ x) [] [(map (\s -> statusAsChar s) srs)]

instance Show SpringRecordNumeric where
  show srn = joinStrings (map (\d -> show d) srn) ',' ""

answerQuestionDayTwelve :: String -> Int
answerQuestionDayTwelve inputText = sum $ map (\r -> countLegalPermutations (num r) (sym r)) records
    where records = parseInput inputText

parseSpringRecordNumeric :: String -> SpringRecordNumeric
parseSpringRecordNumeric s = map (\c -> convertStrToInt c) $ splitStringByAndStripWhiteSpaces "," s

parseSpringRecordSymbols :: String -> SpringRecordSymbols
parseSpringRecordSymbols s = map (\c -> parseCharacterSpringStatus c) s

statusAsChar :: Status -> Char
statusAsChar s
  | s == B = '#'
  | s == O = '.'
  | s == U = '?'

parseCharacterSpringStatus :: Char -> Status
parseCharacterSpringStatus c
  | c == '#' = B
  | c == '.' = O
  | c == '?' = U
  | otherwise = error ("Can't parse symbol " ++ [c] ++ ", not recognized as valid.")

parseLineRecord :: String -> SpringRecord
parseLineRecord l = SpringRecord { num = (parseSpringRecordNumeric numRec), sym = (parseSpringRecordSymbols symRec) }
    where (symRec:numRec:_) = splitStringByAndStripWhiteSpaces " " l

parseInput :: String -> [SpringRecord]
parseInput inputText = map (\l -> parseLineRecord l) $ lines inputText

areRecordsConsistent :: SpringRecordNumeric -> SpringRecordSymbols -> Bool
areRecordsConsistent srn srs =  (map (\g -> length g) $ filter (\c -> c /= "") $ splitStringByAndStripWhiteSpaces "." srsString) == srn
    where srsString = show srs

countLegalPermutations :: SpringRecordNumeric -> SpringRecordSymbols -> Int
countLegalPermutations srn srs = length $ filter (\prm -> areRecordsConsistent srn prm) perms
    where perms = generatePermutations srs

combos :: Int -> [[Int]]
combos 0 = [[]]
combos n = [x : xs | x <- [0, 1], xs <- combos (n - 1)]

isSpringRecordComplete :: SpringRecordSymbols -> Bool
isSpringRecordComplete srs = all (\c -> c /= U) srs

getUPosFromRecord :: SpringRecordSymbols -> [Int]
getUPosFromRecord srs = map (\p -> fst p) $ filter (\p -> (snd p) == U) $ listWithIndex srs

generatePermutations :: SpringRecordSymbols -> [SpringRecordSymbols]
generatePermutations srs
  | isSpringRecordComplete srs = [srs]
  | otherwise = map (\prm -> replaceStatusIterative uPoss prm srs) prms
      where uPoss = getUPosFromRecord srs
            prms = combos $ length uPoss

replaceStatusIterative :: [Int] -> [Int] -> SpringRecordSymbols -> SpringRecordSymbols
replaceStatusIterative [] [] srs = srs
replaceStatusIterative (x:xs) (x':xs') srs
  | x' == 0 = replaceStatusIterative xs xs' (replaceStatusWithBroken srs x)
  | otherwise = replaceStatusIterative xs xs' (replaceStatusWithOperative srs x)

replaceStatusInRecord :: SpringRecordSymbols -> Status -> Int -> SpringRecordSymbols
replaceStatusInRecord srs s i = (take i srs) ++ [s] ++ (drop (i+1) srs)

replaceStatusWithBroken :: SpringRecordSymbols -> Int -> SpringRecordSymbols
replaceStatusWithBroken srs i = replaceStatusInRecord srs B i

replaceStatusWithOperative :: SpringRecordSymbols -> Int -> SpringRecordSymbols
replaceStatusWithOperative srs i = replaceStatusInRecord srs O i