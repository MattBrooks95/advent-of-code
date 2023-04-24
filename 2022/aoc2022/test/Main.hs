import Test.HUnit

import Tests.Day17
import Tests.Day18

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
    ]

main :: IO ()
main = do
    countResults <- runTestTT (TestList (
        testsDay17
        ++ testsDay18
        ))
    print countResults
