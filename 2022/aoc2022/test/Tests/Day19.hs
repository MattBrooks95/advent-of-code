module Tests.Day19 where

import Text.Parsec

import Day19

import Test.HUnit

parsingTests :: Test
parsingTests = TestList [
    parseReqTypes
    , parseCostsTests
    , parseRobotTests
    ]

parseReqTypes :: Test
parseReqTypes = TestList [
    TestCase (
        assertEqual "cost type ore"
        (Right ReqOre)
        (runParser parseReqType () "" "ore")
    )
    , TestCase (
        assertEqual "cost type clay"
        (Right ReqClay)
        (runParser parseReqType () "" "clay")
    )
    , TestCase (
        assertEqual "cost type obsidian"
        (Right ReqOb)
        (runParser parseReqType () "" "obsidian")
    )
    ]

parseCostsTests :: Test
parseCostsTests = TestList [
    TestCase (
        assertEqual "costs 4 ore"
        (Right (CreationRequirement ReqOre 4))
        (runParser parseCost () "" " costs 4 ore")
    )
    , TestCase (
        assertEqual "costs 2 clay"
        (Right (CreationRequirement ReqClay 2))
        (runParser parseCost () "" " costs 2 clay")
    )
    , TestCase (
        assertEqual "costs 1 obsidian"
        (Right (CreationRequirement ReqOb 1))
        (runParser parseCost () "" " costs 1 obsidian")
    )
    ]

parseRobotTests :: Test
parseRobotTests = TestList [
    TestCase (
        assertEqual "geode robot"
        (runParser parseRobotType () "" "geode")
        (Right (Geode, GiveGeodes))
    )
    , TestCase (
        assertEqual "ore robot"
        (runParser parseRobotType () "" "ore")
        (Right (Ore, GiveOre))
    )
    , TestCase (
        assertEqual "obsidian robot"
        (runParser parseRobotType () "" "obsidian")
        (Right (Obsidian, GiveObs))
    )
    , TestCase (
        assertEqual "clay robot"
        (runParser parseRobotType () "" "clay")
        (Right (Clay, GiveClay))
    )
    ]
