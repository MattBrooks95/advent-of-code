module Tests.Day17 where

import Test.HUnit
import Day17

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
