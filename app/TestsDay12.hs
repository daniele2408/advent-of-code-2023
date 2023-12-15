module TestsDay12 where

import Test.HUnit

import qualified Data.Set as DS

import FuncsDay12
import CommonFuncs

testParseNumeric = TestCase(do
      assertEqual "" [5,2,6] (parseSpringRecordNumeric "5,2,6")
  )

testParseSymbols = TestCase(do
      assertEqual "" [B,B,B,U,O,B] (parseSpringRecordSymbols "###?.#")
  )

testParseLine = TestCase(do
      let sr = SpringRecord { num = [5,2,6], sym = [B,B,B,U,O,B]}
      assertEqual "" sr (parseLineRecord "###?.# 5,2,6")
  )

testConsistencyRecords = TestCase(do
      let symRec = parseSpringRecordSymbols "#....######..#####."
      let numRec = parseSpringRecordNumeric "1,6,5"
      let numRec2 = parseSpringRecordNumeric "1,4,5"

      assertEqual "" True (areRecordsConsistent numRec symRec)
      assertEqual "" False (areRecordsConsistent numRec2 symRec)

  )

testReplaceStatus = TestCase(do
      let srs = parseSpringRecordSymbols "?....#?####..#####."

      assertEqual "" "#....#?####..#####." (show $ replaceStatusWithBroken srs 0)
      assertEqual "" "?....#.####..#####." (show $ replaceStatusWithOperative srs 6)
      assertEqual "" "?....#?####..######" (show $ replaceStatusWithBroken srs ((length srs)-1))

  )

testReplaceStatusIterative = TestCase(do
      let srs = parseSpringRecordSymbols "?....#?####..#####."

      let poss = [0, 6]

      assertEqual "" ".....#.####..#####." (show $ replaceStatusIterative poss [1, 1] srs)
      assertEqual "" "#....######..#####." (show $ replaceStatusIterative poss [0, 0] srs)
      assertEqual "" ".....######..#####." (show $ replaceStatusIterative poss [1, 0] srs)
      assertEqual "" "#....#.####..#####." (show $ replaceStatusIterative poss [0, 1] srs)

  )

testGeneratePerm = TestCase(do
      let srs = parseSpringRecordSymbols "?....#?####..#####."

      let perms = generatePermutations srs

      assertEqual "" 4 (length perms)

      let expectedRes = DS.fromList [".....#.####..#####.", "#....######..#####.", ".....######..#####.", "#....#.####..#####."]

      assertEqual "" expectedRes (DS.fromList $ map (\p -> show p) perms)

  )

testCountLegalPerms = TestCase(do
    let records = parseInput "????.######..#####. 1,6,5\n?###???????? 3,2,1"

    assertEqual "" 14 (sum $ map (\r -> countLegalPermutations (num r) (sym r)) records)
  )

testSample = TestCase(do
        inputText <- readFile "./resources/sample/inputday12.txt"

        assertEqual "" 21 (answerQuestionDayTwelve inputText)
  )

testAnswer = TestCase(do
        inputText <- readFile "./resources/inputday12.txt"

        assertEqual "" 21 (answerQuestionDayTwelve inputText)
  )