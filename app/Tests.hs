module Tests where

import Test.HUnit
import Funcs
import FuncsDay2

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

test21 = TestCase(assertEqual "Test day 2" (["ciao", "sono"]) (splitStringBy "," "ciao, sono"))
test211 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces " ciao "))
test212 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces "ciao "))
test213 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces " ciao"))
test214 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces "ciao"))
--test22 = TestCase(assertEqual "Test day 2" (CubeGame 3 0 0) (addRollToGame emptyCubeGame "3 red"))
--test23 = TestCase(assertEqual "Test day 2" (CubeGame 3 2 0) (addRollToGame (addRollToGame emptyCubeGame "3 red") "2 green"))
test25 = TestCase(assertEqual "Test day 2" ("abcdef") (parseLineInput "cappello: abcdef"))
test26 = TestCase(assertEqual "Test day 2" (123) (parseLineGameId "dsads 123: abcdef"))
test28 = TestCase(assertEqual "Test day 2" True (isGamePossibleFor emptyCubeGame (CubeGame 1 2 3)))
test29 = TestCase(assertEqual "Test day 2" False (isGamePossibleFor (CubeGame 3 2 3) (CubeGame 1 2 3)))

testsDay2 = TestList [
    TestLabel "test1" test21,
    TestLabel "test11" test211,
    TestLabel "test12" test212,
    TestLabel "test13" test213,
    TestLabel "test14" test214,
--    TestLabel "test2" test22,
--    TestLabel "test3" test23,
    TestLabel "test4" test25,
    TestLabel "test4" test26,
    TestLabel "test4" test28,
    TestLabel "" (TestCase(assertEqual "Test day 2" False (isPoolLegal (CubeGame 3 2 3) "4 red"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" True (isPoolLegal (CubeGame 3 2 3) "2 green"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" True (areGameSetsLegal (CubeGame 3 2 3) "game 123: 2 green, 3 blue, 3 red"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" False (areGameSetsLegal (CubeGame 3 2 3) "game 123: 0 green, 0 blue, 10 red"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" False (areGameSetsLegal (CubeGame 3 2 3) "game 123: 2 green, 3 blue, 3 red; 0 green, 0 blue, 10 red"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" ([["2 green", "3 blue", "3 red"], ["0 green", "0 blue", "10 red"]]) (parseGameSets "game 123: 2 green, 3 blue, 3 red; 0 green, 0 blue, 10 red"))),
    TestLabel "" (TestCase(assertEqual "Test day 2" (3 * 2 * 3) (powerCubeGame (CubeGame 3 2 3) ))),
    TestLabel "" (TestCase(assertEqual "Test day 2" (CubeGame 10 2 3) (mergeGameSet ["2 green", "3 blue", "3 red", "10 red", "1 green", "3 blue"]))),
    TestLabel "test4" test29]

testAnswersDay2Sample = TestCase(do
  inputText <- readFile "./resources/sample/inputday2.txt"
  assertEqual "" (8) (answerQuestionDayTwo inputText)
  assertEqual "" (2286) (answerQuestionDayTwo' inputText))

testAnswersDay2 = TestCase(do
  inputText <- readFile "./resources/inputday2.txt"
  assertEqual "" (2101) (answerQuestionDayTwo inputText)
  assertEqual "" (58269) (answerQuestionDayTwo' inputText))