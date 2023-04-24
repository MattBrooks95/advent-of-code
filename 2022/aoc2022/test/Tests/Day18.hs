module Tests.Day18 where

import Day18

import qualified Data.Set as S

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

oneCubeCoveredSides :: Test
oneCubeCoveredSides =
    let checkCube = (1, 1, 1)
        cubesList = S.fromList [checkCube, (2, 1, 1)]
    in
        TestCase (
            assertEqual "cube at 1,1,1 with neighbor 2,1,1 has one side covered"
            (getCoveredSidesForCube checkCube cubesList)
            1
        )

twoCubeCoveredSides :: Test
twoCubeCoveredSides =
    let cube1 = (1, 1, 1)
        cube2 = (2, 1, 1)
        cubesList = S.fromList [cube1, cube2]
    in
    TestCase (
        assertEqual "cubes at 1,1,1 and 2,1,1 each have one covered side, for a surface area of 12-2=10"
        (getSurfaceArea cubesList)
        10
    )
