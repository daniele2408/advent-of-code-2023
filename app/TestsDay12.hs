module TestsDay12 where

import Test.HUnit

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

      assertEqual "" True (areRecordsConsistent numRec symRec)

  )