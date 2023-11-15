{-# LANGUAGE OverloadedStrings #-}
module Tests.Day21 (
    tests
    ) where

import Day21 (
    MonkeyType(..)
    , parseLiteralMonkey
    , Expression(..)
    , Op(..)
    , parseExpressionMonkey
    , MonkeyName(..), Monkey (..)
    , parseMonkey
    , solveEquation, ItemOrder (..)
    )


import Test.HUnit

import qualified Data.Attoparsec.ByteString.Lazy as AP

tests :: Test
tests = TestList [
    testParseExpression
    , testParseMonkey
    , testInverseMath
    ]

testParseExpression :: Test
testParseExpression = TestList [
    TestCase (
        assertEqual "parse a literal monkey expression"
        (Right $ LiteralMonkey 1234)
        (AP.parseOnly parseLiteralMonkey "1234")
    )
    , TestCase (
        assertEqual "parse a monkey math expression"
        (Right $ ExpressionMonkey (Expression (MonkeyName "abc") Add (MonkeyName "def")))
        (AP.parseOnly parseExpressionMonkey "abc + def")
    )
    ]

testParseMonkey :: Test
testParseMonkey = TestList [
    TestCase (
        assertEqual "parses an entire monkey line"
        (Right $ Monkey (MonkeyName "dbpl") (LiteralMonkey 5))
        (AP.parseOnly parseMonkey "dbpl: 5")
    )
    , TestCase (
        assertEqual "parses an expression monkey"
        (Right $ Monkey
            (MonkeyName "pppw")
            (ExpressionMonkey (Expression (MonkeyName "cczh") Div (MonkeyName "lfqf")))
        )
        (AP.parseOnly parseMonkey "pppw: cczh / lfqf")
    )
    ]

testInverseMath :: Test
testInverseMath = TestList [
    TestCase (
        assertEqual "solves addition"
        20
        (solveEquation KnownToUnknown 30 Add 10)
    )
    , TestCase (
        assertEqual "solves subtraction"
        10
        (solveEquation KnownToUnknown 30 Sub 40)
    )
    , TestCase (
        assertEqual "solves multiplication"
        2
        (solveEquation KnownToUnknown 30 Mult 15)
    )
    , TestCase (
        assertEqual "solves division"
        2
        (solveEquation KnownToUnknown 30 Div 60)
    )
    -- result = <unk> op knownArg
    -- 10 = X + 30
    -- 10 = -20 + 30
    , TestCase (
        assertEqual "solves addition with unknown value on the felt"
        (-20)
        (solveEquation UnknownToKnown 10 Add 30)
    )
    , TestCase (
        assertEqual "solves subtraction with unknown value on the left"
        100
        (solveEquation UnknownToKnown 30 Sub 70)
    )
    , TestCase (
        assertEqual "solves multiplication with unknown value on the left"
        2
        (solveEquation UnknownToKnown 30 Mult 15)
    )
    -- result = <unk> op knownArg
    -- 25 = X / 3
    , TestCase (
        assertEqual "solves division with unknown value on the left"
        75
        (solveEquation UnknownToKnown 25 Div 3)
    )
    ]
