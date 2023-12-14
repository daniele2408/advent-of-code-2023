module TestsDay10 where

import Test.HUnit
import qualified Data.Set as DS

import FuncsDay11
import CommonFuncs

testFiatLux = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let universe = fiatLux inputText
    assertEqual "" 10 (length universe)
    assertEqual "" 10 (length $ universe !! 0)
  )

testFindVoidStripes = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let universe = fiatLux inputText
    assertEqual "" [3, 7] (findVoidStripesPosition universe)
  )

testFindVoidColumns = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let universe = fiatLux inputText
    assertEqual "" [2, 5, 8] (findVoidColumnPosition universe)
  )

testInsertIntoPosition = TestCase(do
    let ls = [32, 123, 115, 63, 74, 5, 6]
    assertEqual "" [32, 123, 115, 63, 74, 100, 5, 6] (insertIntoPosition 5 100 ls)
    assertEqual "" [32, 123, 115, 63, 74, 5, 6, 100] (insertIntoPosition (length ls) 100 ls)
    assertEqual "" [100, 32, 123, 115, 63, 74, 5, 6] (insertIntoPosition 0 100 ls)
  )

testEnlarge = TestCase(do
        inputText <- readFile "./resources/sample/inputday11.txt"
        let u = fiatLux inputText
        let voidColPos = findVoidColumnPosition u
        let largerUniverse = enlargeYourUniverse u voidColPos
        assertEqual "" 13 (length $ largerUniverse !! 0)
    )

testExpandUniverse = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let universe = fiatLux inputText
    let eu = expandUniverse universe

    assertEqual "" 12 (length eu)
    assertEqual "" 13 (length $ eu !! 0)

    )

testInsert = TestCase(do
    let input = "#...#....."
    let output = "#....#......."

    assertEqual "" output (iterativeInsertIntPositions [5, 2, 8] '.' input)

  )

testGetDistance = TestCase(do
      inputText <- readFile "./resources/sample/inputday11.txt"
      let eu = expandUniverse $ fiatLux inputText

      let gs = extractGalaxyCatalog eu

      let g5 = snd $ gs !! 4
      let g9 = snd $ gs !! 8

      assertEqual "" 9 (getDistance g5 g9)

  )

testOrderByDistance = TestCase(do

    inputText <- readFile "./resources/sample/inputday11.txt"
    let eu = expandUniverse $ fiatLux inputText
    let gs = extractGalaxyCatalog eu

    let g5 = snd $ gs !! 4
    let g9 = snd $ gs !! 8

    let neighs = orderNeighboursByDistanceAsc eu g5 g9
    let expectedRes = [(8,Cell {coords = Coords {x = 2, y = 6}, value = Void}),(8,Cell {coords = Coords {x = 1, y = 7}, value = Void}),(10,Cell {coords = Coords {x = 0, y = 6}, value = Void}),(10,Cell {coords = Coords {x = 1, y = 5}, value = Void})]

    assertEqual "" expectedRes neighs

  )


testCountDistance = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let eu = expandUniverse $ fiatLux inputText

    let gs = extractGalaxyCatalog eu

    let g5 = snd $ gs !! 4
    let g9 = snd $ gs !! 8

    assertEqual "" 9 (countDistance eu g5 g9)
    assertEqual "" 9 (countDistance eu g9 g5)
  )

testGeneratePairs = TestCase(do
    assertEqual "" (DS.fromList [(1,2), (1,3), (2,3)]) (DS.fromList $ generateUniquePairs [1,2,3] [])
  )

testNewGetDistance = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let u = fiatLux inputText

    assertEqual "" 1030 (computeSumDistanceByFactor u 10)
    assertEqual "" 8410 (computeSumDistanceByFactor u 100)
  )


testAnswerSample = TestCase(do
    inputText <- readFile "./resources/sample/inputday11.txt"
    let result = answerQuestionDayEleven inputText

    assertEqual "" 374 result

  )

testAnswer = TestCase(do
    inputText <- readFile "./resources/inputday11.txt"
    let result = answerQuestionDayEleven inputText
    let result' = answerQuestionDayEleven' inputText

    assertEqual "" 9550717 result
    assertEqual "" 648458253817 result'

  )