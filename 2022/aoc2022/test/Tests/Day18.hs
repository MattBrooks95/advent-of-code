module Tests.Day18 where

import Day18

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Data.Maybe

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
genGraphItems dim color = concat [
    concat [[ ((x, y, z), color)
    | z <- coordRange]
    | y <- coordRange]
    | x <- coordRange]
    where
        coordRange = [1..dim]

allCubesSmall :: (Cube -> Int, Int, V.Vector ColoredGraphItem, Cube -> Bool, Cube -> [Cube])
allCubesSmall =
    let dim = 3
        items = genGraphItems dim IsCube
        maxIdx = getIdx (dim, dim, dim)
        empty = V.replicate (maxIdx + 1) ((-1, -1, -1), IsUnchecked)
        getIdx = getIndexForLocation dim
        cubeInBounds = cubeIsInBounds getIdx (dimIsInBounds dim) maxIdx
        getNhbrsGuard = getIndicesOfNeighborsGuard cubeInBounds
    in (
        getIdx
        , maxIdx
        , empty V.// map (\item@((x, y, z), _) -> (getIdx (x, y, z), item)) items
        , cubeInBounds
        , getNhbrsGuard
        )

solidCube :: Test
solidCube =
    let (getIdx, maxIndex, smallTest, _, getNhbrsGuard) = allCubesSmall
    in
    TestList [
        TestCase (
            assertEqual "max index is 26"
            26
            maxIndex
            )
        , TestCase (
            assertEqual "3x3 solid cube center is completely covered"
            0
            (getSurfaceAreaPart2 getIdx getNhbrsGuard [(2, 2, 2)] smallTest)
        )
        , TestCase (
            assertEqual "cube on the corner has 3 showing sides"
            3
            (getSurfaceAreaPart2 getIdx getNhbrsGuard [(1, 1, 1)] smallTest)
        )
        , TestCase (
            assertEqual "cube on the side-middle has 1 showing sides"
            1
            (getSurfaceAreaPart2 getIdx getNhbrsGuard [(2, 2, 1)] smallTest)
        )
    ]

runPart2Sa :: [Cube] -> Int
runPart2Sa cubes = let (sa, _, _) = runPart2 cubes in sa

testPart2 :: Test
testPart2 = TestList [
    --TestCase (
    --        assertEqual "two non-touching cubes have a surface area of 12"
    --        12
    --        (runPart2Sa [
    --            (1, 1, 1)
    --            , (3, 3, 3)
    --            ])
    --    )
    --, TestCase (
    --    assertEqual "two touching cubes have a surface area of 10"
    --    10
    --    (runPart2Sa [
    --        (1,1,1)
    --        , (1,1,2)
    --        ])
    --    )
    --, TestCase (
    --    assertEqual "3x3 cube has a surface area of 54"
    --    54
    --    (let cubes = genCube 3 in runPart2Sa cubes)
    --    )
    --, TestCase (
    --    assertEqual "3x3 cube with a hole in the middle, still has SA of 54"
    --    54
    --    (let cubes = L.delete (2, 2, 2) (genCube 3) in
    --        runPart2Sa cubes
    --        )
    --    )
    --, TestCase (
    --        assertEqual "3x3 cube with a hole in the middle of a side, SA becomes 58"
    --        58
    --        (let cubes = L.delete (1, 2, 2) (genCube 3) in
    --            runPart2Sa cubes
    --            )
    --        )
    --, TestCase (
    --        assertEqual "3x3 cube with a corner removed, SA stays 54"
    --        54
    --        (let cubes = L.delete (1, 1, 1) (genCube 3) in
    --            runPart2Sa cubes
    --            )
    --        )
    --, TestCase (
    --        assertEqual "3x3 cube with a piece on the edge removed, SA becomes 56"
    --        56
    --        (let cubes = L.delete (2, 1, 1) (genCube 3) in
    --            runPart2Sa cubes
    --            )
    --        )
    --, TestCase (
    --        assertEqual "3x3 cube with a hole through the center, SA becomes 64"
    --        64
    --        (let cubes = genCube 3 L.\\ [(2, 2, 1), (2, 2, 2), (2, 2, 3)] in
    --            runPart2Sa cubes
    --            )
    --        )
    --, TestCase (
    --        assertEqual "4x4 cube, SA is 96"
    --        96
    --        (let cubes = genCube 4 in
    --            runPart2Sa cubes
    --            )
    --        )
    --, TestCase (
    --        assertEqual "4x4 cube with a 3x3 cube cut out of it, SA is still 96"
    --        96
    --        (
    --        let outerCubes = genCube 4
    --            innerCubes = map (\(x, y, z) -> (x + 1, y + 1, z + 1)) (genCube 3) in
    --            runPart2Sa $ outerCubes L.\\ innerCubes
    --            )
    --        )
    --, TestCase (
    --    assertEqual "4x4 cube with a peice on the edge removed"
    --    98
    --    (
    --    let outerCubes = genCube 4
    --        cutCubes = [
    --            (2, 1, 1)
    --            ]
    --        cubes = outerCubes L.\\ cutCubes
    --    in
    --        runPart2Sa cubes
    --        )
    --    )
    --, TestCase (
    --    assertEqual "4x4 cube with a peice on the interior of the right side removed"
    --    100
    --    (
    --        let outerCubes = genCube 4
    --            cutCubes = [
    --                (4, 2, 2)
    --                ]
    --            cubes = outerCubes L.\\ cutCubes
    --        in
    --            runPart2Sa (trace
    --                ("outerCubes:" ++ show (length outerCubes) ++ " after removal:" ++ show (length cubes))
    --                trace (show cubes) cubes
    --                )
    --    )
    --    )
    ]

testPart2Indexing :: Test
testPart2Indexing = let getIdx = getIndexForLocation 5 in TestList [
    --TestCase (
    --    assertEqual (mkmsg 0 0 0 0)
    --    0
    --    (getIdx (0, 0, 0))
    --    )
    --, TestCase (
    --    assertEqual (mkmsg 0 0 1 1)
    --    1
    --    (getIdx (0, 0, 1))
    --)
    --, TestCase (
    --    assertEqual (mkmsg 0 1 0 5)
    --    5
    --    (getIdx (0, 1, 0))
    --    )
    --, TestCase (
    --    assertEqual (mkmsg 1 0 0 25)
    --    25
    --    (getIdx (1, 0, 0))
    --    )
    --, TestCase (
    --    assertEqual (mkmsg 1 0 1 26)
    --    26
    --    (getIdx (1, 0, 1))
    --    )
    ---- I think the problem is that if the dimension is 5x5x5
    ---- having a value of 5 for any of the directions should be impossible
    ---- because of zero indexing: 0, 1, 2, 3, 4
    --, TestCase (
    --    assertEqual (mkmsg 0 4 0 20)
    --    20
    --    (getIdx (0, 4, 0))
    --    )
    ---- should be impossible to have a location of 5
    ----, TestCase (
    ----    assertBool "2,2,0 and 2,1,5 have different indexes"
    ----    (let idx1 = getIdx (2, 2, 0); idx2 = getIdx (2, 1, 5);
    ----        in
    ----        (trace ("\n" ++ show idx1 ++ "/=" ++ show idx2) idx1) /= idx2)
    ----    )
    ]
    where
        mkmsg :: Int -> Int -> Int -> Int -> String
        mkmsg x y z a = "idx of " ++ show (x, y, z) ++ " is:" ++ show a

testPart2Interesting :: Test
testPart2Interesting = let (sa, getIdx, graph) = runPart2 (genCube 4 L.\\ [(2, 2, 1)]) in
    --trace ("graph:" ++ show graph) $ TestList [
    TestList [
        --TestCase (
        --    assertBool "idx of (1, 1, 1) is 0"
        --    (getIdx (1, 1, 1) == 0)
        --    )
        --, (let targ = graph V.!? getIdx (2, 2, 1) in TestCase (
        --    assertBool "(2, 2, 1) exists within the graph)"
        --    (isJust $ trace ("(2, 2, 1) cube:" ++ show targ) targ)
        --    )
        --)
        --, TestCase (
        --    assertEqual "2,2,1 is air"
        --    IsAir
        --    (snd $ graph V.! getIdx(2, 2, 1))
        --    )
        --, TestCase (
        --    assertEqual "removed cube (2,2,1) is counted as being air"
        --    IsAir
        --    (snd $ graph V.! getIdx (2, 2, 1))
        --    )
        --, TestCase (
        --    assertEqual "4x4 cube with a peice on the interior of the right side removed"
        --    100
        --    sa
        --    )
        ]


