module FuncsDay8 where

import Data.Char
import Data.Either
import Data.List
import CommonFuncs
import Debug.Trace

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

takeFirstFromSeq :: [a] -> a
takeFirstFromSeq [] = error "Empty seq"
takeFirstFromSeq xs = head $ take 1 $ xs


followInstructionsIterative :: Instructions -> LinkJunction -> [Direction] -> Int -> Int
followInstructionsIterative i lj ds acc
    | (from lj) == "ZZZ" = acc
    | otherwise = followInstructionsIterative i nextJunction (next ds) (acc+1)
    where nextDirection = takeFirstFromSeq ds -- L | R
          destination = chooseDirection nextDirection (to lj)
          nextJunction = selectJunction i destination

traceFii :: LinkJunction -> Direction -> Int -> String
traceFii lj ds acc = "Current junction: " ++ (show lj) ++ " | Current direction: " ++ (show ds) ++ " | Current tot steps: " ++ (show acc)

parseDirectionFromChar :: Char -> Direction
parseDirectionFromChar c
    | c == 'R' = R
    | c == 'L' = L

answerQuestionDayEight :: String -> Int
answerQuestionDayEight inputText = followInstructionsIterative instructions firstLinkJunction directions 0
    where directions = parseDirectionsFromString $ head $ lines inputText
          instructions = parseInstructionFromString $ drop 2 $ lines inputText
          firstLinkJunction = selectJunction instructions "AAA"

parseDirectionsFromString :: String -> [Direction]
--parseDirectionsFromString s = cycle $ map (\x -> parseDirectionFromChar x) s
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