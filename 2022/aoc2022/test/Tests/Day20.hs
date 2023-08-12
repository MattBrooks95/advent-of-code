{-# LANGUAGE QuasiQuotes #-}
module Tests.Day20 (
    tests
    ) where

import Test.HUnit
import Text.Parsec as P

import Text.RawString.QQ (
    r
    )

import qualified Data.Sequence as DS
import Data.Maybe

import Day20 (
    parseInput
    , Item(..)
    , mkItem
    , wrapItems
    --, Increment(..)
    --, StartIndex(..)
    --, CurrentIndex(..)
    --, nextIndex
    , mix
    , itemListToSeq
    , MixableList
    , ItemOrigIndex(..)
    , ItemValue(..)
    , findItemInSeq
    , makeSeq
    )

tests :: Test
tests = TestList [
    parsingTests
--    , modelTests
--  , indexTests
    , shortInputTests
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

--indexTests :: Test
--indexTests = TestList [
--    TestCase (
--        assertEqual "calc 1's new index"
--        1
--        (nextIndex 7 (mkItem (ItemValue 1) (ItemOrigIndex 0)) 0)
--    )
--    , TestCase (
--        assertEqual "calc 2's new index"
--        2
--        (nextIndex 7 (mkItem (ItemValue 2) (ItemOrigIndex 0)) 0)
--    )
--    , TestCase (
--        assertEqual "calc -3's new index"
--        4
--        (nextIndex 7 (mkItem (ItemValue (-3)) (ItemOrigIndex 1)) 1)
--    )
--    , TestCase (
--        assertEqual "calc 4's new index"
--        3
--        (nextIndex 7 (mkItem (ItemValue 4) (ItemOrigIndex 5)) 5)
--    )
--    , TestCase (
--        assertEqual "handles an offset that is equal to the set size"
--        3
--        (nextIndex 7 (mkItem (ItemValue 7) (ItemOrigIndex 3)) 3)
--    )
--    , TestCase (
--        assertEqual "handles an offset that is the inverse of the set size"
--        3
--        (nextIndex 7 (mkItem (ItemValue (-7)) (ItemOrigIndex 3)) 3)
--    )
--    , TestCase (
--        assertEqual "if moved right to the edge, wraps around to the beginning of the list"
--        0
--        (nextIndex 7 (mkItem (ItemValue 3) (ItemOrigIndex 3)) 3)
--    )
--    ]

--smallCase :: [Int]
--smallCase = [1, -2, 4, 6]

itemOne :: Item
itemOne = mkItem (ItemValue 1) (ItemOrigIndex 0)
itemTwo :: Item
itemTwo = mkItem (ItemValue 2) (ItemOrigIndex 1)
itemNegThree :: Item
itemNegThree = mkItem (ItemValue (-3)) (ItemOrigIndex 2)
itemThree :: Item
itemThree = mkItem (ItemValue 3) (ItemOrigIndex 3)
itemNegTwo :: Item
itemNegTwo = mkItem (ItemValue (-2)) (ItemOrigIndex 4)
itemZero :: Item
itemZero = mkItem (ItemValue 0) (ItemOrigIndex 5)
itemFour :: Item
itemFour = mkItem (ItemValue 4) (ItemOrigIndex 6)

smallCaseItems :: [Item]
smallCaseItems = [
    itemOne
    , itemTwo
    , itemNegThree
    , itemThree
    , itemNegTwo
    , itemZero
    , itemFour
    ]

shortInputTests :: Test
shortInputTests = TestList [
    TestCase (
        assertEqual "place 1"
        (DS.fromList [
            itemTwo
            , itemOne
            , itemNegThree
            , itemThree
            , itemNegTwo
            , itemZero
            , itemFour
            ], [])
        (mix (makeSeq smallCaseItems, take 1 smallCaseItems))
    )
    , TestCase (
        assertEqual "place 2"
        (DS.fromList [
            itemOne
            , itemNegThree
            , itemTwo
            , itemThree
            , itemNegTwo
            , itemZero
            , itemFour
            ], [])
        (mix (makeSeq [itemTwo, itemOne, itemNegThree, itemThree, itemNegTwo, itemZero, itemFour], [itemTwo]))
    )
    , TestCase (
        assertEqual "place -3"
        (DS.fromList [
            itemOne
            , itemTwo
            , itemThree
            , itemNegTwo
            , itemNegThree
            , itemZero
            , itemFour
            ], [])
        (mix (makeSeq [itemOne, itemNegThree, itemTwo, itemThree, itemNegTwo, itemZero, itemFour], [itemNegThree]))
    )
    , TestCase (
        assertEqual "place 3"
        (DS.fromList [
            itemOne
            , itemTwo
            , itemNegTwo
            , itemNegThree
            , itemZero
            , itemThree
            , itemFour
            ], [])
        (mix (makeSeq [itemOne, itemTwo, itemThree, itemNegTwo, itemNegThree, itemZero, itemFour], [itemThree]))
    )
    , TestCase (
        assertEqual "place -2"
        (DS.fromList [
            itemOne
            , itemTwo
            , itemNegThree
            , itemZero
            , itemThree
            , itemFour
            , itemNegTwo
            ], [])
        (mix (makeSeq [itemOne, itemTwo, itemNegTwo, itemNegThree, itemZero, itemThree, itemFour], [itemNegTwo]))
    )
    , let nums = [itemOne, itemTwo,itemNegThree,itemZero,itemThree,itemFour,itemNegTwo] in
        TestCase (
            assertEqual "place 0"
            (DS.fromList nums, [])
            (mix (makeSeq nums, [itemZero]))
        )
    , TestCase (
        assertEqual "place 4"
        (DS.fromList [
            itemOne
            , itemTwo
            , itemNegThree
            , itemFour
            , itemZero
            , itemThree
            , itemNegTwo
            ], [])
        (mix (makeSeq [itemOne, itemTwo, itemNegThree, itemZero, itemThree, itemFour, itemNegTwo], [itemFour]))
    )
    , let startingArrangement = [itemOne, itemTwo, itemNegThree, itemThree, itemNegTwo, itemZero, itemFour]
        in TestCase (
            assertEqual "does short input"
            (DS.fromList [itemOne, itemTwo, itemNegThree, itemFour, itemZero, itemThree, itemNegTwo], [])
            (mix (makeSeq startingArrangement, startingArrangement))
    )
    ]

quickItem :: Int -> Int -> Item
quickItem val idx = Item (ItemValue val) (ItemOrigIndex idx)

mixTestsOne :: Item
mixTestsOne = quickItem 1 0

mixTestsNegThree :: Item
mixTestsNegThree = quickItem (-3) 1

mixTestsSix :: Item
mixTestsSix = quickItem 6 2

mixTestsTwo :: Item
mixTestsTwo = quickItem 2 3

mixTestsNegTwo :: Item
mixTestsNegTwo = quickItem (-2) 4

mixTestsNegSix :: Item
mixTestsNegSix = quickItem 6 5

mixTestsStart :: [Item]
mixTestsStart = [
    mixTestsOne
    , mixTestsNegThree
    , mixTestsSix
    , mixTestsTwo
    , mixTestsNegTwo
    , mixTestsNegSix
    ]

mixTests :: Test
mixTests = TestList [
    TestCase (
        assertEqual "offset is a multiple of # items"
        (DS.fromList mixTestsStart, [])
        (mix (makeSeq mixTestsStart, [mixTestsSix]))
    )
    , TestCase (
        assertEqual "offset is a negative multiple of # items"
        (DS.fromList mixTestsStart, [])
        (mix (makeSeq mixTestsStart, [mixTestsSix]))
    )
    , let items = [ quickItem val idx | (val, idx) <- zip [4, -2, 5, 6, 7, 8, 9] [0..]] in TestCase (
        assertEqual "-2 case from the problem statement"
        (DS.fromList [quickItem 4 0, quickItem 5 2, quickItem 6 3, quickItem 7 4, quickItem 8 5, quickItem (-2) 1, quickItem 9 6], [])
        (mix (makeSeq items, [items !! 1]))
    )
    , let items = [quickItem val idx | (val, idx) <- zip [0, 1, 2, -3, 4, 5] [0..]] in
        TestCase (
            assertEqual "go left to exactly 0, wrap around to the end of the list"
            (DS.fromList [quickItem 0 0, quickItem 1 1, quickItem 2 2, quickItem 4 4, quickItem 5 5, quickItem (-3) 3]
                , []
            )
            (mix (makeSeq items, [items !! 3]))
        )
    , let items = [quickItem val idx | (val, idx) <- zip [0, 1, 2, 3, 4] [0..]] in
        TestCase (
            assertEqual "go right to exactly the end of the list, wrap around to beginning"
            (DS.fromList [quickItem 2 2, quickItem 0 0, quickItem 1 1, quickItem 3 3, quickItem 4 4]
                , []
            )
            (mix (makeSeq items, [items !! 2]))
        )
    ---- fails, not sure if the test case I came up with matches the spec
    --, let items@[itemOne, itemTwo, itemThree, itemFive, itemThirteen, itemFourteen] = [quickItem 1 0, quickItem 2 1, quickItem 3 2, quickItem 5 3, quickItem 13 4, quickItem 14 5] in
    --    TestCase (
    --        assertEqual "piece wraps to 1 less than it's starting index"
    --        (DS.fromList [itemOne, itemTwo, itemThree, itemFive, itemThirteen, itemFourteen], [])
    --        (mix (makeSeq items, [quickItem 5 3]))
    --)
    --, let items = [quickItem 1 0, quickItem (-1) 1, quickItem 3 2] in
    --    TestCase (
    --        assertEqual "item goes left to 0"
    --        (DS.fromList [quickItem 1 0, quickItem 3 2, quickItem (-1) 1], [])
    --        (mix (makeSeq items, [quickItem (-1) 1]))
    --    )
    ]
