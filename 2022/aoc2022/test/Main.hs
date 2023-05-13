import Test.HUnit

import System.Exit

import Tests.Day17
import Tests.Day18
import Tests.Day19

testsDay17 = [
    testVertBar
    , testHorizBar
    , testBox
    , testPlus
    , testBackwardsL
    , doesntHitGround
    , hitsGround
    , outOfBoundsXLeft
    , outOfBoundsXRight
    , inBoundsX
    , inBoundsXEdgeLeft
    , inBoundsXEdgeRight
    , doesNotLandOnOtherRock
    , doesLandOnOtherRock
    , noConflict
    , doesConflict
    ]

testsDay18 = [
    getNeighbors
    , oneCubeCoveredSides
    , testUnchecked
    , solidCube
    , testPart2
    --, testPart2Indexing
    --, testPart2Interesting
    ]

testsDay19 = [
    parsingTests
    ]

main :: IO Int
main = do
    countResults <- runTestTT (TestList (
        testsDay17
        ++ testsDay18
        ++ testsDay19
        ))
    print countResults
    let err = errors countResults
        failed = failures countResults
    if err > 0 || failed > 0
    then exitFailure
    else exitSuccess


