module Tests.Day19 where

import Text.Parsec

import Day19

import Test.HUnit

parsingTests :: Test
parsingTests = TestList [
    parseReqTypes
    , parseCostsTests
    , parseRobotTests
    , parseBlueprintTests
    , simulationTests
    ]

parseReqTypes :: Test
parseReqTypes = TestList [
    TestCase (
        assertEqual "cost type ore"
        (Right (ReqType Ore))
        (runParser parseReqType () "" "ore")
    )
    , TestCase (
        assertEqual "cost type clay"
        (Right (ReqType Clay))
        (runParser parseReqType () "" "clay")
    )
    , TestCase (
        assertEqual "cost type obsidian"
        (Right (ReqType Obsidian))
        (runParser parseReqType () "" "obsidian")
    )
    ]

parseCostsTests :: Test
parseCostsTests = TestList [
    TestCase (
        assertEqual "costs 4 ore"
        (Right (CreationRequirement (ReqType Ore) 4))
        (runParser parseCost () "" " costs 4 ore")
    )
    , TestCase (
        assertEqual "costs 2 clay"
        (Right (CreationRequirement (ReqType Clay) 2))
        (runParser parseCost () "" " costs 2 clay")
    )
    , TestCase (
        assertEqual "costs 1 obsidian"
        (Right (CreationRequirement (ReqType Obsidian) 1))
        (runParser parseCost () "" " costs 1 obsidian")
    )
    ]

parseRobotTests :: Test
parseRobotTests = TestList [
    TestCase (
        assertEqual "geode robot"
        (Right (RobotType Geode, GiveGeodes))
        (runParser parseRobotType () "" "geode")
    )
    , TestCase (
        assertEqual "ore robot"
        (Right (RobotType Ore, GiveOre))
        (runParser parseRobotType () "" "ore")
    )
    , TestCase (
        assertEqual "obsidian robot"
        (Right (RobotType Obsidian, GiveObs))
        (runParser parseRobotType () "" "obsidian")
    )
    , TestCase (
        assertEqual "clay robot"
        (Right (RobotType Clay, GiveClay))
        (runParser parseRobotType () "" "clay")
    )
    , TestCase (
        assertEqual "ore robot with cost"
        (Right $ Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 3] GiveOre)
        (runParser parseRobot () "" " Each ore robot costs 3 ore.")
    )
    ]

lineOneBlueprint :: Blueprint
lineOneBlueprint = BluePrint {
    bpId=1
    , robots = [
        Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 3] GiveOre
        , Robot (RobotType Clay) [CreationRequirement (ReqType Ore) 3] GiveClay
        , Robot (RobotType Obsidian) [CreationRequirement (ReqType Ore) 2, CreationRequirement (ReqType Clay) 15] GiveObs
        , Robot (RobotType Geode) [CreationRequirement (ReqType Ore) 3, CreationRequirement (ReqType Obsidian) 9] GiveGeodes
    ]
    }

parseBlueprintTests :: Test
parseBlueprintTests = TestList [
    TestCase (
        assertEqual "parse first line of input"
        (Right lineOneBlueprint)
        (runParser parseBlueprint () "" "Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 15 clay. Each geode robot costs 3 ore and 9 obsidian.")
    )
    , TestCase (
        assertEqual "parse two line file"
        (Right [
            lineOneBlueprint
            , BluePrint {
                bpId=2
                --Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 5 clay. Each geode robot costs 3 ore and 7 obsidian."
                , robots = [
                    Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 4] GiveOre
                    , Robot (RobotType Clay) [CreationRequirement (ReqType Ore) 4] GiveClay
                    , Robot (RobotType Obsidian) [CreationRequirement (ReqType Ore) 4, CreationRequirement (ReqType Clay) 5] GiveObs
                    , Robot (RobotType Geode) [CreationRequirement (ReqType Ore) 3, CreationRequirement (ReqType Obsidian) 7] GiveGeodes
                ]
            }
            ]
        )
        (runParser Day19.parse () ""
            "Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 15 clay. Each geode robot costs 3 ore and 9 obsidian.\nBlueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 5 clay. Each geode robot costs 3 ore and 7 obsidian.\n"
            )
    )
    ]

resourcesTest :: Test
resourcesTest =
    let dummyResources = (Resources { clayRes=16, oreRes=2, obsRes=0, geodeRes=0 })
    in TestList [
    TestCase (
        assertEqual "add two resources"
         -- I think this is terrible because reordering the record fields will mess up the test result
         -- it's probably more proper to write out the field names
        (Resources 1 1 1 1)
        (addRes (Resources 0 0 0 0) (Resources 1 1 1 1))
    )
    , TestCase (
        assertEqual "add clay"
        (Resources 3 0 0 0)
        (addRes (Resources 3 0 0 0) (Resources 0 0 0 0))
        )
    , TestCase (
        assertEqual "robots give resources"
        (Resources 1 0 1 0)
        (sumGenResources [
            Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 0] GiveOre
            , Robot (RobotType Obsidian) [CreationRequirement (ReqType Ore) 0] GiveObs
            ])
        )
    , TestCase (
        assertEqual "can look up resource amount by resource type: clay"
        16
        (getAvailableResource dummyResources Clay)
    )
    , TestCase (
        assertEqual "can look up resource amount by resource type: ore"
        2
        (getAvailableResource dummyResources Ore)
    )
    , TestCase (
        assertBool "hasResources, true case"
        (hasResources dummyResources (CreationRequirement (ReqType Ore) 16))
    )
    , TestCase (
        assertBool "hasResources, fail case"
        (hasResources dummyResources (CreationRequirement (ReqType Ore) 17))
    )
    ]

oreRobot :: Robot
oreRobot = Robot (RobotType Ore) [CreationRequirement (ReqType Clay) 2] GiveOre

clayRobot :: Robot
clayRobot = Robot (RobotType Clay) [] GiveClay

simulationTests :: Test
simulationTests = TestList [
    TestCase (
        assertEqual "1 clay robot makes resources"
        (Resources 1 0 0 0)
        (sumGenResources [oreRobot])
    )
    , TestCase (
        assertEqual "one clay and one ore robot makes one clay and one ore per tick"
        (Resources 1 1 0 0)
        (sumGenResources [oreRobot, clayRobot])
    )
    , TestCase (
        assertBool "can make robot"
        (canAffordRobot (Resources { oreRes=2, clayRes=2, obsRes=2, geodeRes=2 }) oreRobot)
    )
    , TestCase (
        assertBool "can not make robot"
        (not $ canAffordRobot (Resources { oreRes=0, clayRes=2, obsRes=2, geodeRes=2 }) oreRobot)
    )
    , TestCase (
        assertEqual "can make robots with resources"
        [Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 3] GiveOre, Robot (RobotType Clay) [CreationRequirement (ReqType Ore) 3] GiveClay]
        (genActions lineOneBlueprint (Resources { oreRes=3, clayRes=0, obsRes=0, geodeRes=0 }))
    )
    , TestCase (
        assertEqual "can not make robots with resources"
        []
        (genActions lineOneBlueprint (Resources { oreRes=0, clayRes=0, obsRes=0, geodeRes=0 }))
    )
    ]
--lineOneBlueprint :: Blueprint
--lineOneBlueprint = BluePrint {
--    bpId=1
--    , robots = [
--        Robot (RobotType Ore) [CreationRequirement (ReqType Ore) 3] GiveOre
--        , Robot (RobotType Clay) [CreationRequirement (ReqType Ore) 3] GiveClay
--        , Robot (RobotType Obsidian) [CreationRequirement (ReqType Ore) 2, CreationRequirement (ReqType Clay) 15] GiveObs
--        , Robot (RobotType Geode) [CreationRequirement (ReqType Ore) 3, CreationRequirement (ReqType Obsidian) 9] GiveGeodes
--    ]
--    }
