{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module FuncsDay15 where
import Data.Char (ord)
import CommonFuncs
import Data.List.Split

hashAlgo :: String -> Int
hashAlgo s = hashAlgoString s 0

hashAlgoString :: String -> Int -> Int
hashAlgoString [] acc = acc
hashAlgoString (x:xs) acc = hashAlgoString xs (hashAlgoChar x acc)

hashAlgoChar :: Char -> Int -> Int
hashAlgoChar c acc = (\x -> mod x 256) $ (* 17) $ (+ curVal) $ acc
  where curVal = ord c

answerQuestionDay15 :: String -> Int
answerQuestionDay15 inputText = sum $ map (\w -> hashAlgo w) $ splitOn "," inputText