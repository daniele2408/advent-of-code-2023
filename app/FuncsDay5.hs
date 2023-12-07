module FuncsDay5 where

import Data.Char
import Data.List.Split
import CommonFuncs

type RangeNum = (Int, Int)
type RangeNumMap = [(RangeNum, RangeNum)]

data Category = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location deriving (Show, Eq)

data Matcher = Matcher {
    categorySource :: Category,
    categoryDestination :: Category,
    rangeMap :: RangeNumMap
} deriving (Show, Eq)

rangeToRange :: Int -> RangeNumMap -> Int
rangeToRange i [] = i
rangeToRange i (x:xs)
    | isInRange i srcRange = i - (fst srcRange) + (fst trgRange)
    | otherwise = rangeToRange i xs
    where srcRange = fst x
          trgRange = snd x

isInRange :: Int -> RangeNum -> Bool
isInRange i rn = (&&) (i >= (fst rn)) (i <= (snd rn))

createRangeNumMap :: Int -> Int -> Int -> RangeNumMap
createRangeNumMap source destination step = [((source, (source+step-1)), (destination, (destination+step-1)))]

createRangeNumMapFromString :: String -> RangeNumMap
createRangeNumMapFromString line = createRangeNumMap source destination step
    where (destination:source:step:_) = map (\w -> convertStrToInt w) $ words line

createRangeNumMapIterative :: [String] -> RangeNumMap -> RangeNumMap
createRangeNumMapIterative [] acc = acc
createRangeNumMapIterative (x:xs) acc = createRangeNumMapIterative xs ((createRangeNumMapFromString x) ++ acc)

parseMatcherFromStringBlock :: [String] -> Matcher
parseMatcherFromStringBlock (x:xs) = Matcher cSource cDestination (createRangeNumMapIterative xs [])
    where (cSource, cDestination) = parseCategories x

parseCategories :: String -> (Category, Category)
parseCategories s = ((parseCategoryFromString $ tokens !! 0), (parseCategoryFromString $ tokens !! 2))
    where tokens = splitOn "-" $ head $ (words s)

parseCategoryFromString :: String -> Category
parseCategoryFromString "seed" = Seed
parseCategoryFromString "soil" = Soil
parseCategoryFromString "fertilizer" = Fertilizer
parseCategoryFromString "water" = Water
parseCategoryFromString "light" = Light
parseCategoryFromString "temperature" = Temperature
parseCategoryFromString "humidity" = Humidity
parseCategoryFromString "location" = Location
parseCategoryFromString s = error ("Category not mapped for this string " ++ s)

parseMatchersFromInputText :: String -> [Matcher]
parseMatchersFromInputText inputText = map (\b -> parseMatcherFromStringBlock b) blocks
    where blocks = map (\blockLine -> lines blockLine) $ tail $ parseInputTextInBlocks inputText

selMatcher :: Category -> [Matcher] -> Matcher
selMatcher c ms = head $ take 1 $ filter (\m -> (categorySource m) == c) ms

matchInt :: Matcher -> Int -> Int
matchInt m i = rangeToRange i (rangeMap m)

parseSeeds :: [String] -> [Int]
parseSeeds xs = map (\s -> convertStrToInt s) $ words ((splitOn ":" lineSeeds) !! 1)
    where lineSeeds = head xs

findOverlapBetween :: RangeNum -> RangeNum -> RangeNum
findOverlapBetween rn rn'
  | end < start' = (1, 0)
  | end' < start = (1, 0)
  | otherwise = ((max start start'), (min end end'))
    where start = fst rn
          start' = fst rn'
          end = snd rn
          end' = snd rn'

parseSeedsRange :: [String] -> [RangeNum]
parseSeedsRange xs = map (\p -> ((fst p), (fst p) + (snd p) - 1)) $ zip evenPosInts oddPosInts
    where seeds = parseSeeds xs
          nSeeds = length seeds
          evenPosInts = map (\p -> snd p) $ filter (\p -> (mod (fst p) 2) == 0) $ zip [0..(nSeeds-1)] seeds
          oddPosInts = map (\p -> snd p) $ filter (\p -> (mod (fst p) 2) == 1) $ zip [0..(nSeeds-1)] seeds

runMappingRangeInt :: [Matcher] -> RangeNum -> Int
runMappingRangeInt ms seedRange = do

    let matcherSeed = selMatcher Seed ms
    let overlapRanges = filter (\rn -> [(fst rn)..(snd rn)] /= []) $ map (\srcRange -> findOverlapBetween seedRange srcRange) $ map (\rm -> fst rm) (rangeMap matcherSeed)

    minimum $ concat $ map (\or -> map (\i -> runMapping ms i) [(fst or)..(snd or)]) overlapRanges


runMapping :: [Matcher] -> Int -> Int
runMapping ms seed = do
    let matcherSeed = selMatcher Seed ms
    let matcherSoil = selMatcher Soil ms
    let matcherFertilizer = selMatcher Fertilizer ms
    let matcherWater = selMatcher Water ms
    let matcherLight = selMatcher Light ms
    let matcherTemperature = selMatcher Temperature ms
    let matcherHumidity = selMatcher Humidity ms

    -- TODO fold this ffs
    matchInt matcherHumidity (matchInt matcherTemperature (matchInt matcherLight (matchInt matcherWater (matchInt matcherFertilizer (matchInt matcherSoil (matchInt matcherSeed seed))))))

runMappingOnSeeds :: [Matcher] -> [Int] -> [Int]
runMappingOnSeeds ms seeds = map (\seed -> runMapping ms seed) seeds

parseInputTextInBlocks :: String -> [String]
parseInputTextInBlocks inputText = splitOn "\n\n" inputText

answerQuestionDayFive :: String -> Int
answerQuestionDayFive inputText = minimum $ runMappingOnSeeds (parseMatchersFromInputText $ inputText) seeds
    where seeds = parseSeeds $ parseInputTextInBlocks inputText

answerQuestionDayFive' :: String -> Int
answerQuestionDayFive' inputText = minimum $ map (\sr -> runMappingRangeInt matchers sr) seedsRange
    where seedsRange = parseSeedsRange $ parseInputTextInBlocks inputText
          matchers = parseMatchersFromInputText $ inputText