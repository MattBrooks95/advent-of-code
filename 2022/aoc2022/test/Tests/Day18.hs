module Tests.Day18 where

import Day18

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Debug.Trace

import Test.HUnit

getNeighbors :: Test
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

runPart2Sa = fst . runPart2

testPart2 :: Test
testPart2 = TestList [
    TestCase (
            assertEqual "two non-touching cubes have a surface area of 12"
            12
            (runPart2Sa [
                (1, 1, 1)
                , (3, 3, 3)
                ])
        )
    , TestCase (
        assertEqual "two touching cubes have a surface area of 10"
        10
        (runPart2Sa [
            (1,1,1)
            , (1,1,2)
            ])
        )
    , TestCase (
        assertEqual "3x3 cube has a surface area of 54"
        54
        (let cubes = genCube 3 in runPart2Sa cubes)
        )
    , TestCase (
        assertEqual "3x3 cube with a hole in the middle, still has SA of 54"
        54
        (let cubes = L.delete (2, 2, 2) (genCube 3) in
            runPart2Sa cubes
            )
        )
    , TestCase (
            assertEqual "3x3 cube with a hole in the middle of a side, SA becomes 58"
            58
            (let cubes = L.delete (1, 2, 2) (genCube 3) in
                runPart2Sa cubes
                )
            )
    , TestCase (
            assertEqual "3x3 cube with a corner removed, SA stays 54"
            54
            (let cubes = L.delete (1, 1, 1) (genCube 3) in
                runPart2Sa cubes
                )
            )
    , TestCase (
            assertEqual "3x3 cube with a piece on the edge removed, SA becomes 56"
            56
            (let cubes = L.delete (2, 1, 1) (genCube 3) in
                runPart2Sa cubes
                )
            )
    , TestCase (
            assertEqual "3x3 cube with a hole through the center, SA becomes 64"
            64
            (let cubes = genCube 3 L.\\ [(2, 2, 1), (2, 2, 2), (2, 2, 3)] in
                runPart2Sa cubes
                )
            )
    , TestCase (
            assertEqual "4x4 cube, SA is 96"
            96
            (let cubes = genCube 4 in
                runPart2Sa cubes
                )
            )
    , TestCase (
            assertEqual "4x4 cube with a 3x3 cube cut out of it, SA is still 96"
            96
            (
            let outerCubes = genCube 4
                innerCubes = map (\(x, y, z) -> (x + 1, y + 1, z + 1)) (genCube 3) in
                runPart2Sa $ outerCubes L.\\ innerCubes
                )
            )
    , TestCase (
        assertEqual "4x4 cube with a peice on the edge removed"
        98
        (
        let outerCubes = genCube 4
            cutCubes = [
                (2, 1, 1)
                ]
            cubes = outerCubes L.\\ cutCubes
        in
            runPart2Sa cubes
            )
        )
    , TestCase (
        assertEqual "4x4 cube with a peice on the interior of the right side removed"
        100
        (
            let outerCubes = genCube 4
                cutCubes = [
                    (4, 2, 2)
                    ]
                cubes = outerCubes L.\\ cutCubes
            in
                runPart2Sa (trace
                    ("outerCubes:" ++ show (length outerCubes) ++ " after removal:" ++ show (length cubes))
                    trace (show cubes) cubes
                    )
        )
        )
    , TestCase (
        assertEqual "4x4 cube with a peice on the interior of the right side removed"
        100
        (
            let outerCubes = genCube 4
                cutCubes = [
                    (2, 2, 1) -- removing (4, 4, 2) is correct, but this isn't
                    ]
                cubes = outerCubes L.\\ cutCubes
            in
                let (sa, graph) = runPart2 cubes
                in
                    trace (show graph) sa
        )
        )
    ]
