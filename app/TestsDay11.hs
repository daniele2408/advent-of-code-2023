module TestsDay10 where

import Test.HUnit

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