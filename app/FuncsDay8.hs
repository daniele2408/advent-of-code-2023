module FuncsDay8 where

import Data.Char
import Data.Either
import Data.List
import CommonFuncs

data Direction = L | R deriving (Show, Eq)

data Junction = Junction {
    left :: String,
    right :: String
} deriving (Show, Eq)

data LinkJunction = LinkJunction {
    from :: String,
    to :: Junction
} deriving (Show, Eq)

type Instructions = [LinkJunction]

chooseDirection :: Direction -> Junction -> String
chooseDirection d j
    | d == L = left j
    | d == R = right j

-- to "move" cursor to next element in a list
next :: [a] -> [a]
next seq = tail seq ++ [head seq]

selectJunction :: Instructions -> String -> LinkJunction
selectJunction i s = takeFirstFromSeq $ filter (\x -> (from x) == s) i

selectGhostStartingJunctions :: Instructions -> [LinkJunction]
selectGhostStartingJunctions i = filter (\x -> doesEndInChar (from x) 'A') i

doesEndInChar :: String -> Char -> Bool
doesEndInChar s c = (last $ s) == c

takeFirstFromSeq :: [a] -> a
takeFirstFromSeq [] = error "Empty seq"
takeFirstFromSeq xs = head $ take 1 $ xs

followInstructionsIterativeTarget :: Instructions -> LinkJunction -> [Direction] -> (String -> Bool) -> Int -> Int
followInstructionsIterativeTarget i lj ds isTarget acc
    | isTarget $ (from nextJunction) = acc + 1
    | otherwise = followInstructionsIterativeTarget i nextJunction (next ds) isTarget (acc+1)
    where nextDirection = takeFirstFromSeq ds -- L | R
          destination = chooseDirection nextDirection (to lj)
          nextJunction = selectJunction i destination

parseDirectionFromChar :: Char -> Direction
parseDirectionFromChar c
    | c == 'R' = R
    | c == 'L' = L

answerQuestionDayEight :: String -> Int
answerQuestionDayEight inputText = followInstructionsIterativeTarget instructions firstLinkJunction directions (\x -> x == "ZZZ") 0
    where directions = parseDirectionsFromString $ head $ lines inputText
          instructions = parseInstructionFromString $ drop 2 $ lines inputText
          firstLinkJunction = selectJunction instructions "AAA"

answerQuestionDayEight' :: String -> Int
answerQuestionDayEight' inputText = lcmOnList listCycleTracks
    where directions = parseDirectionsFromString $ head $ lines inputText
          instructions = parseInstructionFromString $ drop 2 $ lines inputText
          startingLinkJunctions = selectGhostStartingJunctions instructions
          listCycleTracks = map (\lk -> followInstructionsIterativeTarget instructions lk directions (\x -> doesEndInChar x 'Z') 0) startingLinkJunctions

lcmOnList :: [Int] -> Int
lcmOnList [] = 0
lcmOnList (x:[]) = x
lcmOnList (x:xs) = foldl lcm x xs

parseDirectionsFromString :: String -> [Direction]
parseDirectionsFromString s = map (\x -> parseDirectionFromChar x) s

parseInstructionFromString :: [String] -> Instructions
parseInstructionFromString xs = map (\l -> parseLinkJunctionFromLine l) xs

parseLinkJunctionFromLine :: String -> LinkJunction
parseLinkJunctionFromLine l = LinkJunction { from = fromString, to = junction }
    where (fromString:toJunction:_) = splitStringByAndStripWhiteSpaces "=" l
          junction = parseJunctionFromString toJunction

parseJunctionFromString :: String -> Junction
parseJunctionFromString s = Junction {left = v1, right = v2}
    where (v1:v2:_) = splitStringByAndStripWhiteSpaces "," $ stripCharacter (stripCharacter s '(') ')'