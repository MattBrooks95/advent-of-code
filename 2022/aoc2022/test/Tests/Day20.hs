{-# LANGUAGE QuasiQuotes #-}
module Tests.Day20 (
    tests
    ) where

import Test.HUnit
import Text.Parsec as P

import Text.RawString.QQ (
    r
    )

import Day20 (
    parseInput
    )

tests :: Test
tests = TestList [
    parsingTests
    ]

shortCaseNumbers :: [Int]
shortCaseNumbers = [1, 2, -3, 3, -2, 0, 4]

shortInput :: String
shortInput = [r|1
2
-3
3
-2
0
4
|]

parsingTests :: Test
parsingTests = TestList [
    TestCase (
        assertEqual "can parse short input"
        (Right shortCaseNumbers)
        (P.runParser parseInput () "" shortInput)
    )
    ]
