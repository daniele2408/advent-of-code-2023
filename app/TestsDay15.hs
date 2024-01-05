module TestsDay15 where

import Test.HUnit

import FuncsDay15

testHashAlgo = TestCase(do

        assertEqual "" 52 (hashAlgo "HASH")

  )

testAnswerSample = TestCase(do
        inputText <- readFile "./resources/sample/inputday15.txt"
        assertEqual "" 1320 (answerQuestionDay15 inputText)
  )

testAnswer = TestCase(do
        inputText <- readFile "./resources/inputday15.txt"
        assertEqual "" 514639 (answerQuestionDay15 inputText)
  )

testAnswerSample' = TestCase(do
        inputText <- readFile "./resources/sample/inputday15.txt"
        assertEqual "" 145 (answerQuestionDay15' inputText)
  )