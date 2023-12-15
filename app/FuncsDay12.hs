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
  show srs = show $ foldl (\acc x -> acc ++ x) "" [(map (\s -> statusAsChar s) srs)]

instance Show SpringRecordNumeric where
  show srn = show $ joinStrings (map (\d -> show d) srn) ',' ""

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
areRecordsConsistent srn srs =  (map (\g -> length g) $ splitStringByAndStripWhiteSpaces "." srsString) == srn
    where srsString = show srs
