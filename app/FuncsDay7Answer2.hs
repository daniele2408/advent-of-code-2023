{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay7Answer2 where

import Data.Char
import Data.Ord
import Data.Maybe
import Data.List
import CommonFuncs

data SeedCard = J | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | T | Q | K | A deriving (Bounded, Enum, Eq, Show)

instance Ord SeedCard where
  compare s s' = compare (fromEnum s) (fromEnum s')

-- HandCard should have length 5, but if I define it as 5-uple compiler will bug me when parsing it using only map
type HandCard = [SeedCard]

instance Ord HandCard where
    compare hc1 hc2
        | (parseType hc1) == (parseType hc2) = compareLexicographically hc1 hc2
        | otherwise = compare (parseType hc1) (parseType hc2)

compareLexicographically :: HandCard -> HandCard -> Ordering
compareLexicographically [] [] = EQ
compareLexicographically _ [] = GT
compareLexicographically [] _ = LT
compareLexicographically (x:xs) (x2:xs2)
    | x == x2 = compareLexicographically xs xs2
    | otherwise = compare x x2

data HandType = HIGH_CARD | ONE_PAIR | TWO_PAIR | THREE_OF | FULL_HOUSE | FOUR_OF | FIVE_OF | NONE deriving (Bounded, Enum, Eq, Show)

instance Ord HandType where
  compare s s' = compare (fromEnum s) (fromEnum s')

parseType :: HandCard -> HandType
parseType hc
    | counts == [5] = FIVE_OF -- QQQQQ
    | counts == [1, 4] = FOUR_OF -- QQQQA
    | counts == [2, 3] = FULL_HOUSE -- QQQAA
    | counts == [1, 1, 3] = THREE_OF -- QQQAK
    | counts == [1, 2, 2] = TWO_PAIR -- QQAAK
    | counts == [1, 1, 1, 2] = ONE_PAIR -- QQATK
    | counts == [1, 1, 1, 1, 1] = HIGH_CARD -- 9QATK
    | otherwise = error ("Sequence " ++ (show counts) ++ " unknown, can't map it to a known HandType.")
    where counts = sort $ extractCountsFromCountSeed $ computeCountSeed $ transmuteJoker hc

type CountSeed = [(SeedCard, Int)]

-- [THREE, TWO, T, THREE, K]
--  [K, K, SIX, SEVEN, SEVEN]
-- [T, FIVE, FIVE, J, FIVE]
--  [K, T, J, J, T]
-- [Q, Q, Q, J, A]

isEqualToJ :: SeedCard -> Bool
isEqualToJ sc = sc == J

transmuteJoker :: HandCard -> HandCard
transmuteJoker hc = result
    where mostFreq = (extractMostFrequentSeed hc)
          result = case mostFreq of
            Just x -> map (\sc -> substituteIf (isEqualToJ) sc x) hc
            Nothing -> hc

substituteIf :: (SeedCard -> Bool) -> SeedCard -> SeedCard -> SeedCard
substituteIf pred sc sc'
    | pred sc = sc'
    | otherwise = sc

extractMostFrequentSeed :: HandCard -> Maybe SeedCard
extractMostFrequentSeed hc
    | sortedListByFrequencyAsc /= [] = Just (fst $ last sortedListByFrequencyAsc)
    | otherwise = Nothing
    where sortedListByFrequencyAsc = sortBy sortPairsBySecond $ computeCountSeed $ filter (\x -> x /= J) hc

sortPairsBySecond (_, a1) (_, a2)
  | a1 > a2 = GT
  | a1 < a2 = LT
  | a1 == a2 = EQ

extractCountsFromCountSeed :: CountSeed -> [Int]
extractCountsFromCountSeed cs = map (\x -> snd x) cs

countDistinctSeeds :: HandCard -> Int
countDistinctSeeds hc = length $ computeCountSeed hc

computeCountSeed :: HandCard -> CountSeed
computeCountSeed hc =  map (\xs@(x:_) -> (x, length xs)) . group . sort $ hc

type Bid = Int
type HandBid = (HandCard, Bid)
type RankedHandBid = (Int, HandBid)

answerQuestionDaySeven :: String -> Int
answerQuestionDaySeven s = computeTotalWinnings $ parseGame s

computeTotalWinnings :: [HandBid] -> Int
computeTotalWinnings hbs = sum $ map (\rhb -> computeWinning rhb) $ zip [1..] $ sortBy (\hb1 hb2 -> compare (fst hb1) (fst hb2)) hbs

computeWinning :: RankedHandBid -> Int
computeWinning rhb = (fst rhb) * (snd $ snd rhb)

parseGame :: String -> [HandBid]
parseGame inputText = map (\l -> parseHandBidFromString l) $ lines inputText

parseHandBidFromString :: String -> HandBid
parseHandBidFromString l = (handPart, bidPart)
    where handPart = parseHandFromString $ (words l) !! 0
          bidPart = convertStrToInt $ (words l) !! 1

parseHandFromString :: String -> HandCard
parseHandFromString s
    | (length s) == 5  = map (\c -> parseSeedFromChar c) s
    | otherwise = error "Irregular string for a hand, should have length 5."

--TODO couldn't it be set as enum's param?
parseSeedFromChar :: Char -> SeedCard
parseSeedFromChar '2' = TWO
parseSeedFromChar '3' = THREE
parseSeedFromChar '4' = FOUR
parseSeedFromChar '5' = FIVE
parseSeedFromChar '6' = SIX
parseSeedFromChar '7' = SEVEN
parseSeedFromChar '8' = EIGHT
parseSeedFromChar '9' = NINE
parseSeedFromChar 'T' = T
parseSeedFromChar 'J' = J
parseSeedFromChar 'Q' = Q
parseSeedFromChar 'K' = K
parseSeedFromChar 'A' = A
parseSeedFromChar c = error $ "Couldn't parse character" ++ [c] ++ " to a known SeedCard"


