module TestsGeneral where

import Test.HUnit
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
import CommonFuncs

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

test21 = TestCase(assertEqual "Test day 2" (["ciao", "sono"]) (splitStringByAndStripWhiteSpaces "," "ciao, sono"))
test211 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces " ciao "))
test212 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces "ciao "))
test213 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces " ciao"))
test214 = TestCase(assertEqual "Test day 2" ("ciao") (stripWhiteSpaces "ciao"))
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

sampleDay2 = TestCase(do
  inputText <- readFile "./resources/sample/inputday2.txt"
  assertEqual "" (8) (answerQuestionDayTwo inputText)
  assertEqual "" (2286) (answerQuestionDayTwo' inputText))

answersDay2 = TestCase(do
  inputText <- readFile "./resources/inputday2.txt"
  assertEqual "" (2101) (answerQuestionDayTwo inputText)
  assertEqual "" (58269) (answerQuestionDayTwo' inputText))

testTakeWhileIsDigit = TestCase(assertEqual "Test take while char is digit" "123" (takeWhileIsDigit "123*..."))
testParseSymbolAndNumber = TestCase(assertEqual "Test parse line 123*..." ([SymbolItem "*" (Point 0 3, Point 0 3), NumberItem 123 (Point 0 0, Point 0 2)]) (parseSchemaLine (0, "123*...") [] 0))

testParseSchemas1 = TestCase(assertEqual "" ([
  NumberItem 32 (Point 0 8, Point 0 9),
  SymbolItem "#" (Point 0 7, Point 0 7),
  NumberItem 456 (Point 0 4, Point 0 6),
  SymbolItem "*" (Point 0 3, Point 0 3),
  NumberItem 123 (Point 0 0, Point 0 2)]) (parseSchemaLine (0, "123*456#32.") [] 0))

testParseSchemas2 = TestCase(assertEqual "" ([
  NumberItem 321 (Point 0 8, Point 0 10),
  SymbolItem "#" (Point 0 7, Point 0 7),
  NumberItem 456 (Point 0 4, Point 0 6),
  SymbolItem "*" (Point 0 3, Point 0 3),
  NumberItem 123 (Point 0 0, Point 0 2)]) (parseSchemaLine (0, "123*456#321") [] 0))

testsDay3 = TestList [
  TestLabel "test1" (TestCase(assertEqual "Test day 3 1 number" ([NumberItem 467 (Point 0 0, Point 0 2)]) (parseSchemaLine (0, "467.....") [] 0))),
  TestLabel "test1" (TestCase(assertEqual "Test day 3 1 symbol" ([SymbolItem "*" (Point 0 0, Point 0 0)]) (parseSchemaLine (0, "*...") [] 0))),
  TestLabel "test1" (TestCase(assertEqual "Test day 3 2" ([NumberItem 467 (Point 0 0, Point 0 2)]) (parseSchemaLine (0, ".") [NumberItem 467 (Point 0 0, Point 0 2)] 10))),
  TestLabel "test1" (TestCase(assertEqual " Test day 3 3" ([
    NumberItem {numberValue = 114, coords = (Point {x = 0, y = 5},Point {x = 0, y = 7})},
    NumberItem {numberValue = 467, coords = (Point {x = 0, y = 0},Point {x = 0, y = 2})}]
    ) (parseSchema $ generateSchemaFromInput "467..114..")))]


sampleDay3 = TestCase(do
  inputText <- readFile "./resources/sample/inputday3.txt"
  assertEqual "" (4361) (answerQuestionDayThree inputText)
  assertEqual "" (467835) (answerQuestionDayThree' inputText))

testParseCardFromLine = TestCase(assertEqual "test parse card from line" (ScratchCard 10 [1, 23, 32, 43] [32, 65, 23, 4]) (parseScratchCardFromLine "Card 10:  1 23 32 43 | 32 65 23  4"))
testComputeCardValue = TestCase(assertEqual "test compute card value" 8 (computeCardValue (ScratchCard 1 [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53])))
testGetMatches = TestCase(assertEqual "test compute tot matches" [83, 86, 17, 48] (getMatchingNumbers (ScratchCard 1 [41, 48, 83, 86, 17] [83, 86, 6, 31, 17, 9, 48, 53])))

testUpdateScratchHolder = TestCase(assertEqual "test scratch holder update" [(2, 4), (1, 1)] (updateScratchCardHolderCopies 2 3 [(1,1), (2, 3)]))

testInitScratchCardHolder = TestCase(
  assertEqual
  "test init scratch card holder"
  [(1,1),(2,1),(3,1)]
  (initScratchCardHolderFrom [ScratchCard 1 [1] [1], ScratchCard 1 [1] [1], ScratchCard 1 [1] [1]])
  )

testGenerateUpdateCommands = TestCase(
  assertEqual
  "test generate commands"
  [(24, 3), (25, 3)]
  (generateUpdateCommands (ScratchCard 23 [1, 2, 3] [11, 2, 3]) 3)
  )

testGenerateUpdateCommands2 = TestCase(
  assertEqual
  "test generate commands"
  [(13, 2), (14, 2), (15, 2)]
  (generateUpdateCommands (ScratchCard 12 [1, 2, 3] [1, 2, 3]) 2)
  )

testApplyUpdateCommands = TestCase(
   assertEqual
   "test apply commands"
   [(3, 3), (2, 8), (1, 2)]
   (applyUpdateCommands [(1, 1), (2, 3), (3, 4)] [(1, 1), (2, 7), (3, 2)])
  )

testApplyGenerate = TestCase(
  assertEqual
  ""
  [(5,11),(4,11),(1,10),(2,10),(3,10)]
  (applyUpdateCommands (generateUpdateCommands (ScratchCard 3 [1, 2, 3] [1, 2, 3]) 1) [(1,10), (2,10), (3,10), (4,10), (5,10)])
  )

testUpdateCopiesIterative = TestCase(
  assertEqual
  "test update copies"
  [(3,3),(2,2),(1,1)]
  (updateCopiesIterative [
      ScratchCard 1 [1, 2, 3] [1, 2, 3],
      ScratchCard 2 [1, 2, 3] [12, 2, 3],
      ScratchCard 3 [10, 11] [12, 2, 3]
    ] (initScratchCardHolderFrom [
                                       ScratchCard 1 [1, 2, 3] [1, 2, 3],
                                       ScratchCard 2 [1, 2, 3] [12, 2, 3],
                                       ScratchCard 3 [10, 11] [12, 2, 3]
                                     ]))
  )

sampleDay4 = TestCase(do
    inputText <- readFile "./resources/sample/inputday4.txt"
    assertEqual "" (13) (answerQuestionDayFour inputText))


answersDay4 = TestCase(do
    inputText <- readFile "./resources/inputday4.txt"
    assertEqual "" (20855) (answerQuestionDayFour inputText)
    assertEqual "" (5489600) (answerQuestionDayFour' inputText)
  )

testSplitInputFileInBlock = TestCase(do
    inputText <- readFile "./resources/sample/inputday5.txt"
    let blockText = parseInputTextInBlocks inputText
    assertEqual "" 8 (length $ blockText)
    assertEqual "" "seeds: 79 14 55 13" (head blockText)
    assertEqual "" "humidity-to-location map:" ((lines $ blockText !! 7) !! 0)
    )

testParseCategories = TestCase(
    assertEqual "" (Seed, Soil) (parseCategories "seed-to-soil map:")
    )

testSplitInputFileInBlock2 = TestCase(do
    inputText <- readFile "./resources/sample/inputday5.txt"
    let lastBlockText = lines $ (parseInputTextInBlocks inputText) !! 7
    assertEqual "" (Matcher Humidity Location [((93,93+4-1), (56,56+4-1)), ((56,56+37-1),(60,60+37-1))] ) (parseMatcherFromStringBlock lastBlockText)
    )

testRangeToRange = TestCase(do
    inputText <- readFile "./resources/sample/inputday5.txt"
    assertEqual "" 1 (rangeToRange 1 [((93,93+4-1), (56,56+4-1)), ((56,56+37-1),(60,60+37-1))])
    )

testRangeToRange2 = TestCase(
    assertEqual "" 57 (rangeToRange 94 [((93,93+4-1), (56,56+4-1)), ((56,56+37-1),(60,60+37-1))])
    )

testSelectMatcherByCategory = TestCase(
    assertEqual "" (Matcher Light Temperature [((93,93+4-1), (56,56+4-1))]) (selMatcher Light [
        Matcher Light Temperature [((93,93+4-1), (56,56+4-1))],
        Matcher Soil Seed [((93,93+4-1), (56,56+4-1))],
        Matcher Water Light [((93,93+4-1), (56,56+4-1))]
    ]))

sampleDay5 = TestCase(do
  inputText <- readFile "./resources/sample/inputday5.txt"
  assertEqual "" (35) (answerQuestionDayFive inputText)
  assertEqual "" (46) (answerQuestionDayFive' inputText)

  )

answersDay5 = TestCase(do
    inputText <- readFile "./resources/inputday5.txt"
    assertEqual "" (340994526) (answerQuestionDayFive inputText)
--    assertEqual "" (340994526) (answerQuestionDayFive' inputText)
  )

testParseTime = TestCase(do
    assertEqual "" [10, 21, 23, 56] (parseTimesFromInputText "Time:      10 21 23 56")
    )

testParseRaceRecords = TestCase(do
    inputText <- readFile "./resources/sample/inputday6.txt"
    assertEqual "" [(7, 9), (15, 40), (30, 200)] (parseRaceRecords inputText)
    )

testParseRaceRecord = TestCase(do
    inputText <- readFile "./resources/sample/inputday6.txt"
    assertEqual "" (71530, 940200) (parseRaceRecord inputText)
    )

sampleDay6 = TestCase(do
  inputText <- readFile "./resources/sample/inputday6.txt"
  assertEqual "" (288) (answerQuestionDaySix inputText)
  assertEqual "" (71503) (answerQuestionDaySix' inputText)
  )

testHandCardParse = TestCase(do
    assertEqual "" [A, A, J, Q, Q] (parseHandFromString "AAJQQ")
    assertEqual "" [A, A, J, THREE, Q] (parseHandFromString "AAJ3Q")
    assertEqual "" [A, FIVE, J, Q, Q] (parseHandFromString "A5JQQ")
    )

testHandBidParse = TestCase(do
    assertEqual "" ([A, A, J, Q, Q], 12) (parseHandBidFromString "AAJQQ 12")
    assertEqual "" ([A, A, J, THREE, Q], 435) (parseHandBidFromString "AAJ3Q 435")
    )

testParseGame = TestCase(do
    inputText <- readFile "./resources/sample/inputday7.txt"
    assertEqual "" [([THREE,TWO,T,THREE,K],765),([T,FIVE,FIVE,J,FIVE],684),([K,K,SIX,SEVEN,SEVEN],28),([K,T,J,J,T],220),([Q,Q,Q,J,A],483)] (parseGame inputText)
    )

testCompWin = TestCase(do
    assertEqual "" 10 (computeWinning (2, ([THREE,TWO], 5)))
    )

testsSampleDay7 = TestCase(do
    inputText <- readFile "./resources/sample/inputday7.txt"
    --assertEqual "" 6440 (answerQuestionDaySeven inputText)
    assertEqual "" 5905 (answerQuestionDaySeven inputText)
    )

testsDay7 = TestCase(do
    inputText <- readFile "./resources/inputday7.txt"
    assertEqual "" 241344943 (answerQuestionDaySeven inputText)
    assertEqual "" 243101568 (FDSEVEN.answerQuestionDaySeven inputText)
    )

testChooseDirection = TestCase(do
    assertEqual "" "A" (chooseDirection L Junction{ left="A", right="B"})
    assertEqual "" "B" (chooseDirection R Junction{ left="A", right="B"})
    )

testNext = TestCase(do
    assertEqual "" 2 (takeFirstFromSeq (next $ cycle [1,2,3,4]))
    assertEqual "" 3 (takeFirstFromSeq (next $ next $ cycle [1,2,3]))
    assertEqual "" 1 (takeFirstFromSeq (next $ next $ next $ cycle [1,2,3]))
    assertEqual "" 1 (takeFirstFromSeq (next $ cycle [1]))
    )

testParseJunction = TestCase(do
    assertEqual "" Junction{ left="AAA", right="BBB" } (parseJunctionFromString "(AAA, BBB)")
    )

testParseLinkJunction = TestCase(do
    assertEqual "" LinkJunction{ from = "CCC", to = Junction{ left="AAA", right="BBB" }} (parseLinkJunctionFromLine "CCC = (AAA, BBB)")
    )

testSelectGhostJunctions = TestCase(do
    inputText <- readFile "./resources/inputday8.txt"
    assertEqual "" 6 (length $ selectGhostStartingJunctions $ parseInstructionFromString $ drop 2 $ lines inputText)
    )

testsSampleDay8 = TestCase(do
    inputText <- readFile "./resources/sample/inputday8.txt"
    inputTextBis <- readFile "./resources/sample/inputday8bis.txt"
    inputTextTris <- readFile "./resources/sample/inputday8tris.txt"
    assertEqual "" 2 (answerQuestionDayEight inputText)
    assertEqual "" 6 (answerQuestionDayEight inputTextBis)
    assertEqual "" 6 (answerQuestionDayEight' inputTextTris)
    )

testLcmOnList = TestCase(do
        assertEqual "" 336 (lcmOnList [12, 16, 14])
    )

testAnswersDay8 = TestCase(do
    inputText <- readFile "./resources/sample/inputday8.txt"
    assertEqual "" 15871 (answerQuestionDayEight inputText)
    assertEqual "" 11283670395017 (answerQuestionDayEight' inputText)
    )

testParseInputSeq = TestCase(do
    assertEqual "" IntSequence{ elements = [10,23,45], lastElem = -1 } (parseInputSeq "10 23 45")
    )

testComputeDiffSeq = TestCase(do
    let seq_ = IntSequence{ elements = [0,3,6,9,12,15], lastElem = -1 }
    assertEqual "" IntSequence{ elements = [3,3,3,3,3], lastElem = -1 } (computeDiffSeq seq_)
    )

testSampleAnswersDay9 = TestCase(do
    inputText <- readFile "./resources/sample/inputday9.txt"
    assertEqual "" 114 (answerQuestionDayNine inputText)
    )

testAnswersDay9 = TestCase(do
    inputText <- readFile "./resources/inputday9.txt"
    assertEqual "" 1987402313 (answerQuestionDayNine inputText)
    assertEqual "" 900 (answerQuestionDayNine' inputText)
    )
