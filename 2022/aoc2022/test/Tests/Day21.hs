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
    , solveEquation
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
        (solveEquation 30 Add 10)
    )
    , TestCase (
        assertEqual "solves subtraction"
        10
        (solveEquation 30 Sub 40)
    )
    , TestCase (
        assertEqual "solves multiplication"
        2
        (solveEquation 30 Mult 15)
    )
    , TestCase (
        assertEqual "solves division"
        2
        (solveEquation 30 Div 60)
    )
    ]
