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
    , Item(..)
    , mkItem
    , wrapItems
    --, Increment(..)
    --, StartIndex(..)
    --, CurrentIndex(..)
    , nextIndex
    , mix
    , itemListToSeq
    , MixableList
    )

tests :: Test
tests = TestList [
    parsingTests
--    , modelTests
    , indexTests
    , mixTests
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

--modelTests :: Test
--modelTests = TestList [
--    TestCase (
--        assertEqual "mkItem"
--        (Item (Increment 10) (StartIndex 0) (CurrentIndex 0))
--        (mkItem (Increment 10) (StartIndex 0))
--    )
--    , TestCase (
--        assertEqual "numbers are wrapped into Items"
--        [mkItem (Increment 1) (StartIndex 0)
--        , mkItem (Increment 2) (StartIndex 1)
--        , mkItem (Increment (-3)) (StartIndex 2)
--        , mkItem (Increment 3) (StartIndex 3)
--        , mkItem (Increment (-2)) (StartIndex 4)
--        , mkItem (Increment 0) (StartIndex 5)
--        , mkItem (Increment 4) (StartIndex 6)
--        ]
--        (wrapItems shortCaseNumbers)
--    )
--    ]

indexTests :: Test
indexTests = TestList [
    TestCase (
        assertEqual "calc 1's new index"
        1
        (nextIndex 7 (mkItem 1) 0)
    )
    , TestCase (
        assertEqual "calc 2's new index"
        2
        (nextIndex 7 (mkItem 2) 0)
    )
    , TestCase (
        assertEqual "calc -3's new index"
        4
        (nextIndex 7 (mkItem (-3)) 1)
    )
    , TestCase (
        assertEqual "calc 4's new index"
        3
        (nextIndex 7 (mkItem 4) 5)
    )
    ]

numberListToItemSeq :: [Int] -> Day20.MixableList
numberListToItemSeq = itemListToSeq . wrapItems

mixTests :: Test
mixTests = TestList [
    TestCase (
        assertEqual "-2 wraps around to end of list"
        (numberListToItemSeq [1, 2, -3, 0, 3, 4, -2], wrapItems [])
        (mix (numberListToItemSeq [1, 2, -2, -3, 0, 3, 4], wrapItems [-2]))
    )
    ]
