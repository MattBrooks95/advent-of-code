module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

import Data.List (
    intercalate
    )

data Giveable = GiveOre | GiveClay | GiveObs | GiveGeodes deriving (Show, Eq)

data ReqType = ReqOre | ReqClay | ReqOb
    deriving (Show, Eq)
data CreationRequirement = CreationRequirement ReqType Int
    deriving (Show, Eq)

data RobotType = Ore | Clay | Obsidian | Geode
    deriving (Show, Eq)

data Resources = Resources {
    oreRes :: Int
    , clayRes :: Int
    , obsRes :: Int
    , geodeRes :: Int
} deriving (Show, Eq)

emptyResources :: Resources
emptyResources = Resources {
    oreRes=0
    , clayRes=0
    , obsRes=0
    , geodeRes=0
}

addRes :: Resources -> Resources -> Resources
addRes (Resources { oreRes=ore1, clayRes=clay1, obsRes=obs1, geodeRes=geode1 }) (Resources { oreRes=ore2, clayRes=clay2, obsRes=obs2, geodeRes=geode2}) =
    Resources {
        oreRes = ore1 + ore2
        , clayRes = clay1 + clay2
        , obsRes = obs1 + obs2
        , geodeRes = geode1 + geode2
    }

data Robot = Robot RobotType [CreationRequirement] Giveable
    deriving (Show, Eq)

makeOreRobot :: CreationRequirement -> Robot
makeOreRobot req = Robot Ore [req] GiveOre

data Simulation = Simulation {
    blueprint :: Blueprint
    , rbts :: [Robot]
    , resources :: Resources
    , timeRemaining :: Int
    }

sumGenResources :: [Robot] -> Resources
sumGenResources currRobots = let madeRes = genResources currRobots in foldl addRes emptyResources madeRes

genResources :: [Robot] -> [Resources]
genResources = map getRes

getRes :: Robot -> Resources
getRes (Robot _ _ GiveOre) = Resources { oreRes=1, clayRes=0, obsRes=0, geodeRes=0 }
getRes (Robot _ _ GiveClay) = Resources { oreRes=0, clayRes=1, obsRes=0, geodeRes=0 }
getRes (Robot _ _ GiveObs) = Resources { oreRes=0, clayRes=0, obsRes=1, geodeRes=0 }
getRes (Robot _ _ GiveGeodes) = Resources { oreRes=0, clayRes=0, obsRes=0, geodeRes=1 }

data Blueprint = BluePrint {
    bpId :: Int
    , robots :: [Robot]
    } deriving (
         Eq
        )
instance Show Blueprint where
    show bp = "\n(BP " ++ show (bpId bp)
        ++ "\n\t" ++ intercalate "\n\t" (map show (robots bp))
        ++ "\n)"

run :: String -> IO ()
run input = do
    print "day19"
    case P.runParser parse () "" input of
        Left e -> print e
        Right blueprints -> do
            print blueprints


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
    _ <- P.string " robot"
    firstCost <- parseCost
    additionalCosts <- P.many parseAdditionalCost --TODO do I need a try here?
    _ <- P.char '.'
    return $ Robot robotType (firstCost:additionalCosts) giveable

parseBlueprint :: P.Parsec String () Blueprint
parseBlueprint = do
    _ <- P.string "Blueprint "
    thisBpId <- PS.digits
    _ <- P.string ":"
    thisRobots <- P.many parseRobot
    return BluePrint {
        robots=thisRobots
        , bpId=thisBpId
    }

parse :: P.Parsec String () [Blueprint]
--still need to figure out why just P.sepBy doesn't work
--parse = (parseBlueprint `P.sepBy` P.endOfLine) <* P.eof
parse = P.many (parseBlueprint <* P.endOfLine) <* P.eof
