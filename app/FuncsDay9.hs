module FuncsDay9 where

import Data.Char
import Data.List
import CommonFuncs

data IntSequence = IntSequence {
    elements :: [Int],
    lastElem :: Int
} deriving (Show, Eq)

type IntSequenceCollection = [IntSequence]

answerQuestionDayNine :: String -> Int
answerQuestionDayNine inputText = sum $ map (\seq_ -> resolveIntSequence seq_ [seq_]) seqs
    where seqs = parseInputText inputText

resolveIntSequence :: IntSequence -> IntSequenceCollection -> Int
resolveIntSequence seq_ acc
    | isSeqAllZero seq_ = computeLastElems acc 0
    | otherwise = resolveIntSequence diffSeq (diffSeq:acc)
        where diffSeq = computeDiffSeq seq_

computeLastElems :: IntSequenceCollection -> Int -> Int
computeLastElems (x:xs) i
    | xs == [] = i
    | otherwise = computeLastElems xs (nextSecondLastElem + i)
        where nextSecondLastElem = last $ elements $ head xs

isSeqAllZero :: IntSequence -> Bool
isSeqAllZero seq_ = all (\x -> x == 0) $ elements seq_

resolveLastElem :: IntSequence -> IntSequence -> IntSequence
resolveLastElem seqToSolve seqSolved
    | lastElemSolved == -1 = error $ "Empty last elem for sequence " ++ (show seqSolved)
    | otherwise = IntSequence {elements = (elements seqToSolve), lastElem = ((last $ elements seqToSolve) + lastElemSolved) }
        where lastElemSolved = lastElem seqSolved

computeDiffSeq :: IntSequence -> IntSequence
computeDiffSeq seq_ = IntSequence { elements = newElements, lastElem = (lastElem seq_) }
    where is = (elements seq_)
          allButFirst = tail is
          allButLast = keepAllButLast is
          newElements = map (\p -> (fst p) - (snd p)) $ zip allButFirst allButLast

keepAllButLast :: [a] -> [a]
keepAllButLast [] = []
keepAllButLast [x] = []
keepAllButLast (x:xs) = (x:(reverse $ tail $ reverse xs))

parseInputSeq :: String -> IntSequence
parseInputSeq s = IntSequence { elements = is, lastElem = -1 }
    where is = map (\x -> convertStrToInt x) $ words s

parseInputText :: String -> [IntSequence]
parseInputText inputText = map (\l -> parseInputSeq l) $ lines inputText