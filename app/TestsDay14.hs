module TestsDay14 where

import Test.HUnit

import FuncsDay14
import CommonFuncs

testSampleAnswer = TestCase(do
        inputText <- readFile "./resources/sample/inputday14.txt"

        assertEqual "" 136 (answerQuestionDayFourteen inputText)

  )

testAnswer = TestCase(do
        inputText <- readFile "./resources/inputday14.txt"

        assertEqual "" 112773 (answerQuestionDayFourteen inputText)

  )