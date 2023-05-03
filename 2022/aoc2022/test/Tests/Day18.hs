module Tests.Day18 where

import Day18

import qualified Data.Set as S
import qualified Data.Vector as V

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

colorChecks :: [(ColoredLoc, Bool)]
colorChecks = [
    (IsUnchecked, True)
    , (IsAir, False)
    , (IsChecking, False)
    , (IsCube, False)
    ]

testUnchecked :: Test
testUnchecked = TestList $ map (\(color, answer) -> TestCase $ assertEqual ("color:" ++ show color) (isUnchecked color) answer) colorChecks

genGraphItems :: Int -> ColoredLoc -> [ColoredGraphItem]
genGraphItems dim color = concat [ concat [[ ((x, y, z), color) | z <- coordRange] | y <- coordRange] | x <- coordRange]
    where
        coordRange = [0..dim + 1]

allCubes :: Int -> [ColoredGraphItem]
allCubes dim = genGraphItems dim IsCube

allCubesSmall :: (Cube -> Int, Int, V.Vector ColoredGraphItem)
allCubesSmall =
    let dim = 3
        items = genGraphItems dim IsCube
        maxIdx = getIdx (dim + 1, dim + 1, dim + 1)
        empty = V.replicate (maxIdx + 1) ((-1, -1, -1), IsUnchecked)
        getIdx = getIndexForLocation (dim + 1)
    in (getIdx, maxIdx, empty V.// map (\item@((x, y, z), _) -> (getIdx (x, y, z), item)) items)

solidCube :: Test
solidCube =
    let (getIdx, maxIdx, smallTest) = allCubesSmall
    in
    TestList [
        TestCase (
            assertEqual "3x3 solid cube center is completely covered"
            0
            (getSurfaceAreaPart2 getIdx [(2, 2, 2)] smallTest)
        )
        , TestCase (
            assertEqual "cube on the corner has 3 showing sides"
            3
            (getSurfaceAreaPart2 getIdx [(0, 0, 0)] smallTest)
        )
        , TestCase (
            assertEqual "cube on the side-middle has 1 showing sides"
            1
            (getSurfaceAreaPart2 getIdx [(0, 1, 0)] smallTest)
        )
    ]

testPart2 :: Test
testPart2 = TestCase (
        assertEqual "two non-touching cubes have a surface area of 12"
        12
        (runPart2 [
        (1, 1, 1)
        , (3, 3, 3)
        ])
    )
