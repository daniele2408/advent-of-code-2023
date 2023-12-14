module TestsDay10 where

import Test.HUnit

import FuncsDay10
import CommonFuncs
import Data.Set

testCollectGroundTiles = TestCase(do
    inputText <- readFile "./resources/sample/inputday10.txt"
    let gc = parseGridFromInputText inputText
    let Just tc = getTileCell gc Coords { x = 2, y = 1}
    let Just nextTc = getTileCell gc Coords { x = 3, y = 1}
    let ncc = collectGroundTiles gc tc
    let gcc = selectGroundTiles tc nextTc ncc emptyGcc
    let tot = (length $ left gcc) + (length $ right gcc)
    assertEqual "" 4 tot
  )

testWhichDirection = TestCase(do
        inputText <- readFile "./resources/sample/inputday10.txt"
        let gc = parseGridFromInputText inputText
        let Just t2 = getTileCell gc Coords { x = 2, y = 1 }
        let Just t3 = getTileCell gc Coords { x = 3, y = 1 }
        let Just t4 = getTileCell gc Coords { x = 3, y = 2 }

        assertEqual "" (Just W) (whichDirectionIsTile t2 t3)
        assertEqual "" (Just E) (whichDirectionIsTile t3 t2)
        assertEqual "" (Just S) (whichDirectionIsTile t4 t3)
        assertEqual "" (Just N) (whichDirectionIsTile t3 t4)
    )

testGetTileDirection = TestCase(do
        inputText <- readFile "./resources/sample/inputday10.txt"
        let gc = parseGridFromInputText inputText
        let Just centralTile = getTileCell gc Coords { x = 2, y = 2 }
        assertEqual "" (Just (TileCell { tile = (E,W), coords = Coords { x = 2, y = 1 }})) (getNorthCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (E,W), coords = Coords { x = 2, y = 3 }})) (getSouthCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (N,S), coords = Coords { x = 3, y = 2 }})) (getEastCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (N,S), coords = Coords { x = 1, y = 2 }})) (getWestCell gc (Just centralTile))

        assertEqual "" (Just (TileCell { tile = (START,START), coords = Coords { x = 1, y = 1 }})) (getNorthWestCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (S,W), coords = Coords { x = 3, y = 1 }})) (getNorthEastCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (N,E), coords = Coords { x = 1, y = 3 }})) (getSouthWestCell gc (Just centralTile))
        assertEqual "" (Just (TileCell { tile = (N,W), coords = Coords { x = 3, y = 3 }})) (getSouthEastCell gc (Just centralTile))

    )

testAddTurn = TestCase(do
    assertEqual "" (TurnCounter { rightTurns = 101, leftTurns = 10 }) (addTurn R TurnCounter { rightTurns = 100, leftTurns = 10 } )
    assertEqual "" (TurnCounter { rightTurns = 100, leftTurns = 11 }) (addTurn L TurnCounter { rightTurns = 100, leftTurns = 10 } )
    )

testTurnCounter = TestCase(do
        inputText <- readFile "./resources/sample/inputday10.txt"
        let gc = parseGridFromInputText inputText
        let Just t1 = getTileCell gc Coords { x = 1, y = 1 }
        let Just t2 = getTileCell gc Coords { x = 2, y = 1 }
        let Just t3 = getTileCell gc Coords { x = 3, y = 1 }
        let Just t4 = getTileCell gc Coords { x = 3, y = 2 }

        assertEqual "" (TurnCounter { rightTurns = 0, leftTurns = 0}) (addTurnMovingFromTo t1 t2 TurnCounter { rightTurns = 0, leftTurns = 0})
        assertEqual "" (TurnCounter { rightTurns = 0, leftTurns = 0}) (addTurnMovingFromTo t2 t3 TurnCounter { rightTurns = 0, leftTurns = 0})
        assertEqual "" (TurnCounter { rightTurns = 0, leftTurns = 1}) (addTurnMovingFromTo t3 t2 TurnCounter { rightTurns = 0, leftTurns = 0})
        assertEqual "" (Just S) (whichDirectionIsTile t4 t3)
        assertEqual "" (TurnCounter { rightTurns = 1, leftTurns = 0}) (addTurnMovingFromTo t3 t4 TurnCounter { rightTurns = 0, leftTurns = 0})
  )

testGetPerimeter = TestCase(do
                inputText <- readFile "./resources/sample/inputday10.txt"
                let gc = parseGridFromInputText inputText
                let Just tc = getTileCell gc Coords { x = 2, y = 2 }
                assertEqual "" (fromList [tc]) (startFindAreaPerimeter gc)
    )

allTests = TestList [
    TestLabel "turnCounter" testTurnCounter,
    TestLabel "testAddTurn" testAddTurn,
    TestLabel "testCollectGroundTiles" testCollectGroundTiles,
    TestLabel "testGetTileDirection" testGetTileDirection,
    TestLabel "testWhichDirection" testWhichDirection
    ]