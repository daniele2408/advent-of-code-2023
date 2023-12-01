module Tests where

import Test.HUnit
import Funcs

test1 = TestCase(assertEqual "" (72) (extractFirstAndLastNumber "7fjqhrhsevenlbtwoninevnmct2"))
test2 = TestCase(assertEqual "" (95) (extractFirstAndLastNumber "gmlqzxdxtt9five"))
test3 = TestCase(assertEqual "" (45) (extractFirstAndLastNumber "foursevenjqncdtkqxg65hcqxrssvlq"))
test4 = TestCase(assertEqual "" (59) (extractFirstAndLastNumber "56nine9one8fourcpnine"))

test4bis = TestCase(assertEqual "" "9" (extractLastNumber "56nine9one8fourcpnine"))

testAnswer1Day1 = TestCase(do
  inputText <- readFile "./resources/inputday1.txt"
  assertEqual "" (53080) (answerQuestionDayOne inputText))

testAnswer2Day1 = TestCase(do
  inputText <- readFile "./resources/inputday1.txt"
  assertEqual "" (53268) (answerQuestionDayOne' inputText))

testsDay1 = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test4bis" test4bis]

testAnswers = TestList [TestLabel "a1d1" testAnswer1Day1, TestLabel "a2d1" testAnswer2Day1]

-- HOW TO: run 'cabal repl' ':l app/Tests.hs' 'runTestTT <variableTestList>'