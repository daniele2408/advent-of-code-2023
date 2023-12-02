module Main where

import Funcs
import FuncsDay2

main :: IO ()
main = do
    inputText <- readFile "./resources/inputday1.txt"

    putStrLn $ "The sum of all calibration values is " ++ (show $ answerQuestionDayOne inputText) ++ "."
    putStrLn $ "The sum of all calibration values, considering number words as well, is " ++ (show $ answerQuestionDayOne' inputText) ++ "."

    inputText2 <- readFile "./resources/inputday2.txt"
    putStrLn $ "The sum of valid games' ids is " ++ (show $ answerQuestionDayTwo inputText2)
    putStrLn $ "The sum of the power of possible games is " ++ (show $ answerQuestionDayTwo' inputText2)