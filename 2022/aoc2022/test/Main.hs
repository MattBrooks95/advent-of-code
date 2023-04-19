import Test.HUnit

import Tests.Day17

testsDay17 = TestList [
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

main :: IO ()
main = do
    countResults <- runTestTT testsDay17
    print countResults
