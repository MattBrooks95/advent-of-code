module Tests.Day18 where

import Day18

import Test.HUnit

getNeighbors = TestCase (
    assertEqual "box at 1 1 1's neighbors"
    (getIndicesOfNeighbors (1, 1, 1))
    [ (2, 1, 1)
    , (1, 2, 1)
    , (1, 1, 2)
    , (0, 1, 1)
    , (1, 0, 1)
    , (1, 1, 0)
    ]
    )
