module Main where

import Funcs
import FuncsDay2
import FuncsDay3
import FuncsDay4
import FuncsDay5
import FuncsDay6

main :: IO ()
main = do
    inputText <- readFile "./resources/inputday1.txt"
    putStrLn $ ">>>>>>>>>>>>>>> DAY 1 <<<<<<<<<<<<<<<"
    putStrLn $ "The sum of all calibration values is " ++ (show $ answerQuestionDayOne inputText) ++ "."
    putStrLn $ "The sum of all calibration values, considering number words as well, is " ++ (show $ answerQuestionDayOne' inputText) ++ "."

    putStrLn $ ">>>>>>>>>>>>>>> DAY 2 <<<<<<<<<<<<<<<"
    inputText2 <- readFile "./resources/inputday2.txt"
    putStrLn $ "The sum of valid games' ids is " ++ (show $ answerQuestionDayTwo inputText2)
    putStrLn $ "The sum of the power of possible games is " ++ (show $ answerQuestionDayTwo' inputText2)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 3 <<<<<<<<<<<<<<<"
    inputText3 <- readFile "./resources/inputday3.txt"
    putStrLn $ "The sum of all of the part numbers in the engine schematic is " ++ (show $ answerQuestionDayThree inputText3)
    putStrLn $ "The sum of all of the gear ratios in the engine schematic is " ++ (show $ answerQuestionDayThree' inputText3)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 4 <<<<<<<<<<<<<<<"
    inputText4 <- readFile "./resources/inputday4.txt"
    putStrLn $ "The scratchcards are worth " ++ (show $ answerQuestionDayFour inputText4) ++ " points."
    putStrLn $ "I'll end up having " ++ (show $ answerQuestionDayFour' inputText4) ++ " scratchcards."
    putStrLn $ ">>> " ++ (show $ answerQuestionDayFour inputText4)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 5 <<<<<<<<<<<<<<<"
    inputText5 <- readFile "./resources/inputday5.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDayFive inputText5)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 6 <<<<<<<<<<<<<<<"
    inputText6 <- readFile "./resources/inputday6.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDaySix inputText6)
    putStrLn $ ">>> " ++ (show $ answerQuestionDaySix' inputText6)
