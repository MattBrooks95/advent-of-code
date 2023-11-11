{-# LANGUAGE OverloadedStrings #-}
module Tests.Day21 (
    tests
    ) where

import Day21 (
    MonkeyType(..)
    , parseLiteralMonkey
    )


import Test.HUnit

import qualified Data.Attoparsec.ByteString.Lazy as AP

tests :: Test
tests = TestList [
    testParseExpression
    ]

testParseExpression :: Test
testParseExpression = TestList [
    TestCase (
        assertEqual "parse a literal monkey expression"
        (Right $ LiteralMonkey 1234)
        (AP.parseOnly parseLiteralMonkey "1234")
    )
    ]

