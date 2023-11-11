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
    )


import Test.HUnit

import qualified Data.Attoparsec.ByteString.Lazy as AP

tests :: Test
tests = TestList [
    testParseExpression
    , testParseMonkey
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
    ]
