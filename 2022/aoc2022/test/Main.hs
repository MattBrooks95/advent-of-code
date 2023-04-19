import Test.HUnit

import Tests.Day17

testsDay17 = TestList [
    testVertBar
    , testHorizBar
    , testBox
    , testPlus
    , testBackwardsL
    ]

main :: IO ()
main = do
    countResults <- runTestTT testsDay17
    print countResults
