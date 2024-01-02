module TestsDay14 where

import Test.HUnit

import FuncsDay14
import CommonFuncs
import Data.Map (Map)
import qualified Data.Map as Map

testSampleAnswer = TestCase(do
        inputText <- readFile "./resources/sample/inputday14.txt"

        assertEqual "" 136 (answerQuestionDayFourteen inputText)

  )

testAnswer = TestCase(do
        inputText <- readFile "./resources/inputday14.txt"

        assertEqual "" 112773 (answerQuestionDayFourteen inputText)

  )

testSampleAnswer' = TestCase(do
        inputText <- readFile "./resources/sample/inputday14.txt"

        assertEqual "" 136 (answerQuestionDayFourteen' inputText)

  )

--testAcc = TestCase(do
--
--    let modZeros = [(1,103868),(3,103830),(4,103798),(7,103761),(9,103758),(15,103714),(19,103283),(24,102811),(31,102179),(39,101832),(49,101419),(63,99947),(79,98813),(99,98887),(124,98899),(127,98883),(159,98889),(199,98885),(249,98875),(255,98898),(319,98887),(399,98898),(499,98889),(511,98889),(624,98874),(639,98885),(799,98875),(999,98884)]
--
--
--
--  )
