module Main where

import Funcs
import FuncsDay2
import FuncsDay3
import FuncsDay4
import FuncsDay5
import FuncsDay6
import FuncsDay7
import qualified FuncsDay7Answer2 as FDSEVEN
import FuncsDay8
import FuncsDay9
import FuncsDay10
import FuncsDay11

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

    putStrLn $ ">>>>>>>>>>>>>>> DAY 7 <<<<<<<<<<<<<<<"
    inputText7 <- readFile "./resources/inputday7.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDaySeven inputText7)
    putStrLn $ ">>> " ++ (show $ FDSEVEN.answerQuestionDaySeven inputText7)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 8 <<<<<<<<<<<<<<<"
    inputText8 <- readFile "./resources/inputday8.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDayEight inputText8)
    putStrLn $ ">>> " ++ (show $ answerQuestionDayEight' inputText8)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 9 <<<<<<<<<<<<<<<"
    inputText9 <- readFile "./resources/inputday9.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDayNine inputText9)
    putStrLn $ ">>> " ++ (show $ answerQuestionDayNine' inputText9)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 10 <<<<<<<<<<<<<<<"
    inputText10 <- readFile "./resources/inputday10.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDayTen inputText10)
    putStrLn $ ">>> " ++ (show $ answerQuestionDayTen' inputText10)

    putStrLn $ ">>>>>>>>>>>>>>> DAY 11 <<<<<<<<<<<<<<<"
    inputText11 <- readFile "./resources/inputday11.txt"
    putStrLn $ ">>> " ++ (show $ answerQuestionDayEleven inputText11)
    putStrLn $ ">>> " ++ (show $ answerQuestionDayEleven' inputText11)