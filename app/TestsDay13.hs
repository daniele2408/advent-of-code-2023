module TestsDay13 where

import Test.HUnit

import FuncsDay13
import CommonFuncs

testGenerateGrid = TestCase(do
        inputText <- readFile "./resources/sample/inputday13.txt"
        let grids = generateGrids inputText
        assertEqual "" 2 (length grids)

        let gridA = grids !! 0
        let gridB = grids !! 1

        assertEqual "" 9 (maxWidth gridA)
        assertEqual "" 7 (maxHeight gridA)

        assertEqual "" 9 (maxWidth gridB)
        assertEqual "" 7 (maxHeight gridB)

        assertEqual "" [2] (findAllSameRowPos gridA)
        assertEqual "" [3] (findAllSameRowPos gridB)

  )

testListOutwards = TestCase(do

    let l = take 10 $ repeat 1

    assertEqual "" [(1,2), (0,3)] (listPairWiseOutwardPositions l 1)
    assertEqual "" [(2,3), (1,4), (0,5)] (listPairWiseOutwardPositions l 2)

    )

testIsValidHorizReflectionPos = TestCase(do

    inputText <- readFile "./resources/sample/inputday13.txt"
    let grids = generateGrids inputText
    assertEqual "" 2 (length grids)

    let gridA = grids !! 0
    let gridB = grids !! 1

    let reflectionPosA = (!! 0) $ findAllSameRowPos $ gridA
    let reflectionPosB = (!! 0) $ findAllSameRowPos $ gridB

    assertEqual "" False (isValidHorizReflectionPos gridA reflectionPosA)
    assertEqual "" False (isValidHorizReflectionPos gridA (reflectionPosA+1))

    assertEqual "" True (isValidHorizReflectionPos gridB reflectionPosB)
    assertEqual "" False (isValidHorizReflectionPos gridB (reflectionPosB+1))

    )

testGetReflectionPos = TestCase(do

    inputText <- readFile "./resources/sample/inputday13.txt"
    let grids = generateGrids inputText
    assertEqual "" 2 (length grids)

    let gridA = grids !! 0
    let gridB = grids !! 1

    assertEqual "" Nothing (getHorizReflectionPos gridA)
    assertEqual "" (Just 3) (getHorizReflectionPos gridB)

    assertEqual "" (Just 4) (getVertReflectionPos gridA)
    assertEqual "" Nothing (getVertReflectionPos gridB)

    )



testSample = TestCase(do
        inputText <- readFile "./resources/sample/inputday13.txt"

        assertEqual "" 405 (answerQuestionDayThirteen inputText)

  )

testAnswer = TestCase(do
    inputText <- readFile "./resources/inputday13.txt"

    assertEqual "" 31265 (answerQuestionDayThirteen inputText)


  )

testAnswer' = TestCase(do
    inputText <- readFile "./resources/inputday13.txt"

    assertEqual "" 39359 (answerQuestionDayThirteen' inputText)

  )