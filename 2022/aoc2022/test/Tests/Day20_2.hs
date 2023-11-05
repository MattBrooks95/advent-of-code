module Tests.Day20_2 (
    tests
    ) where

import qualified Data.Sequence as S

import Test.HUnit

import Day20_2 (
    MixItem(..)
    , MixItemValue(..)
    , MixItemIndex(..)
    , mix'
    )

one, two, negThree, three, negTwo, zero, four :: MixItem
one = MixItem (MixItemValue 1, MixItemIndex 0)
two = MixItem (MixItemValue 2, MixItemIndex 1)
negThree = MixItem (MixItemValue (-3), MixItemIndex 2)
three = MixItem (MixItemValue 3, MixItemIndex 3)
negTwo = MixItem (MixItemValue (-2), MixItemIndex 4)
zero = MixItem (MixItemValue 0, MixItemIndex 5)
four = MixItem (MixItemValue 4, MixItemIndex 6)

startingSeq :: S.Seq MixItem
startingSeq = S.fromList [one, two, negThree, three, negTwo, zero, four]

mixOne, mixTwo, mixThree, mixFour, mixFive, mixSix, mixSeven :: S.Seq MixItem
mixOne = S.fromList [two, one, negThree, three, negTwo, zero, four]
mixTwo = S.fromList [one, negThree, two, three, negTwo, zero, four]
mixThree = S.fromList [one, two, three, negTwo, negThree, zero, four]
mixFour = S.fromList [one, two, negTwo, negThree, zero, three, four]
mixFive = S.fromList [one, two, negThree, zero, three, four, negTwo]
mixSix = S.fromList [one, two, negThree, zero, three, four, negTwo]
mixSeven = S.fromList [one, two, negThree, four, zero, three, negTwo]

tests :: Test
tests = TestList [
    TestCase (
        assertEqual "mix 1"
        (Right mixOne)
        (mix' startingSeq [one])
    )
    , TestCase (
        assertEqual "mix 2"
        (Right mixTwo)
        (mix' mixOne [two])
    )
    , TestCase (
        assertEqual "mix 3"
        (Right mixThree)
        (mix' mixTwo [negThree])
    )
    , TestCase (
        assertEqual "mix 4"
        (Right mixFour)
        (mix' mixThree [three])
    )
    --, TestCase (
    --    assertEqual "mix 5"
    --    (Right mixFive)
    --    (mix' mixFour [negTwo])
    --)
    --, TestCase (
    --    assertEqual "mix 6"
    --    (Right mixSix)
    --    (mix' mixFive [zero])
    --)
    --, TestCase (
    --    assertEqual "mix 7"
    --    (Right mixSeven)
    --    (mix' mixSix [four])
    --)
    ]
