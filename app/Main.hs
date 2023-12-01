module Main where

import Funcs

main :: IO ()
main = do
    inputText <- readFile "./resources/inputday1.txt"

    putStrLn $ "The sum of all calibration values is " ++ (show $ answerQuestionDayOne inputText) ++ "."
    putStrLn $ "The sum of all calibration values, considering number words as well, is " ++ (show $ answerQuestionDayOne' inputText) ++ "."
