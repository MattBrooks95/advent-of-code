module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

data Giveable = GiveOre | GiveClay | GiveObs | GiveGeodes deriving (Show, Eq)

data ReqType = ReqOre | ReqClay | ReqOb
    deriving (Show, Eq)
data CreationRequirement = CreationRequirement ReqType Int
    deriving (Show, Eq)

data RobotType = Ore | Clay | Obsidian | Geode
    deriving (Show, Eq)

data Robot = Robot RobotType [CreationRequirement] Giveable
    deriving (Show)

makeOreRobot :: CreationRequirement -> Robot
makeOreRobot req = Robot Ore [req] GiveOre

data Blueprint = BluePrint {
    bpId :: Int
    , robots :: [Robot]
    } deriving (
        Show
        )

run :: String -> IO ()
run input = do
    print "day19"
--    case P.runParser parseBlueprint () "" input of
--        Left e -> print e
--        Right blueprint -> do
--            print blueprint


parseObs :: P.Parsec String () String
parseObs = P.string "obsidian"

parseClay :: P.Parsec String () String
parseClay = P.string "clay"

parseOre :: P.Parsec String () String
parseOre = P.string "ore"

parseGeode :: P.Parsec String () String
parseGeode = P.string "geode"

parseReqType :: P.Parsec String () ReqType
parseReqType = P.choice [
    P.try parseOre >> return ReqOre
    , parseObs >> return ReqOb
    , parseClay >> return ReqClay
    ]

parseCostDetails :: P.Parsec String () CreationRequirement
parseCostDetails = PS.integer >>= \num ->
    PS.plainWhitespace >> parseReqType >>= \costType ->
        return $ CreationRequirement costType num

parseCost :: P.Parsec String () CreationRequirement
parseCost = P.string " costs " >> parseCostDetails

parseAdditionalCost :: P.Parsec String () CreationRequirement
parseAdditionalCost = P.string " and " >> parseCostDetails

parseRobotType :: P.Parsec String () (RobotType, Giveable)
parseRobotType =
    P.choice [
        P.try parseOre >> return (Ore, GiveOre)
        , parseClay >> return (Clay, GiveClay)
        , parseObs >> return (Obsidian, GiveObs)
        , parseGeode >> return (Geode, GiveGeodes)
        ]

parseRobot :: P.Parsec String () Robot
parseRobot = do
    _ <- P.string " Each "
    (robotType, giveable) <- parseRobotType
    firstCost <- parseCost
    additionalCosts <- P.many parseAdditionalCost
    return $ Robot robotType (firstCost:additionalCosts) giveable

--parseBlueprint :: P.Parsec String () Blueprint
--parseBlueprint = do
--    _ <- P.string "Blueprint "
--    bpId <- digits
--    _ <- string ":"
--    robots <- P.many parseRobot
--    undefined
--
--parse :: String -> P.Parsec String () [Blueprint]
--parse input = (P.many parseBlueprint `P.sepBy` P.endOfLine) <* P.eof
