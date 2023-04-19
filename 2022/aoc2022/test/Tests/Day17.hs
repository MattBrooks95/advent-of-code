module Tests.Day17 where

import Test.HUnit
import Day17

import qualified Data.Map as M

testVertBar = TestCase (
    assertEqual "vertBar at 0 0"
    (verticalBar 0 0)
    [(0, 0), (0, 1), (0, 2), (0, 3)]
    )

testHorizBar = TestCase (
    assertEqual "horiz bar at 0 0"
    (horizBar 0 0)
    [(0, 0), (1, 0), (2, 0), (3, 0)]
    )

testBox = TestCase (
    assertEqual "box at 0 0"
    (box 0 0)
    [(0, 0), (0, 1), (1, 0), (1, 1)]
    )

testPlus = TestCase (
    assertEqual "plus at 0 0"
    (plusSign 0 0)
    [(0, 1), (1, 1), (2, 1), (1, 0), (1, 2)]
    )

testBackwardsL = TestCase (
    assertEqual "backwards l at 0 0"
    (backwardsL 0 0)
    [(0, 0), (1, 0), (2,0), (2, 1), (2, 2)]
    )

testParse = TestCase (
    assertEqual "parsing >><"
    (doParse ">><\n")
    (Right [JLeft, JLeft, JRight])
    )

doesntHitGround = TestCase (
    assertBool "does not hit ground"
    (not $ isDoneFalling 0 M.empty (0, 1))
    )

hitsGround = TestCase (
    assertBool "does hit the ground"
    (isDoneFalling 0 M.empty (0, 0))
    )

settledRock = M.fromList [((2, 1), ())]

doesNotLandOnOtherRock = TestCase (
    assertBool "rock does not land on other rock"
    (not $ isDoneFalling 0 settledRock (2, 2))
    )

doesLandOnOtherRock = TestCase (
    assertBool "rock lands on other rock"
    (isDoneFalling 0 settledRock (2, 1))
    )

outOfBoundsXLeft = TestCase (
    assertBool "out of bounds on x axis"
    (isIllegal (-1, 0))
    )

outOfBoundsXRight = TestCase (
    assertBool "out of bounds on x axis"
    (isIllegal (7, 0))
    )

inBoundsX = TestCase (
    assertBool "is in bounds"
    (not $ isIllegal (3, 0))
    )

inBoundsXEdgeLeft = TestCase (
    assertBool "is barely in bounds on the left"
    (not $ isIllegal (0, 0))
    )

inBoundsXEdgeRight = TestCase (
    assertBool "is barely in bounds on the left"
    (not $ isIllegal (0, 0))
    )

cross :: M.Map Location ()
cross = M.fromList (zip [(2, 1), (2, 2), (2, 3), (1, 2), (3, 2)] (repeat ()))

noConflict = TestCase (
    assertBool "does not conflict with other rock"
    (not $ locationsConflict [(1, 3)] cross)
    )

doesConflict = TestCase (
    assertBool "conflicts with other rock"
    (locationsConflict [(2, 3)] cross)
    )
