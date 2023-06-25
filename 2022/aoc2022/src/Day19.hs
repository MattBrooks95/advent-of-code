module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

import Debug.Trace

import Data.List (
    intercalate
    , maximumBy
    )

import Data.Function (
    on
    )

data Giveable = GiveOre | GiveClay | GiveObs | GiveGeodes deriving (Show, Eq)

data Resource = Ore | Clay | Obsidian | Geode
    deriving (Show, Eq)

newtype ReqType = ReqType Resource
    deriving (Show, Eq)

data CreationRequirement = CreationRequirement ReqType Int
    deriving (Show, Eq)

newtype RobotType = RobotType Resource
    deriving (Show, Eq)

data Resources = Resources {
    oreRes :: Int
    , clayRes :: Int
    , obsRes :: Int
    , geodeRes :: Int
} deriving (Show, Eq)

getAvailableResource :: Resources -> Resource -> Int
getAvailableResource (Resources { oreRes=availOre} ) Ore = availOre
getAvailableResource (Resources { clayRes=availClay} ) Clay = availClay
getAvailableResource (Resources { obsRes=availObs} ) Obsidian = availObs
getAvailableResource (Resources { geodeRes=availGeode }) Geode = availGeode

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
makeOreRobot req = Robot (RobotType Ore) [req] GiveOre

data Simulation = Simulation {
    blueprint :: Blueprint
    , rbts :: [Robot]
    , resources :: Resources
    , timeRemaining :: Int
    }
    deriving (Show)

time :: Simulation -> Simulation
time s@(Simulation { timeRemaining=rTime }) = s { timeRemaining=rTime - 1 }

addResources :: Simulation -> Resources -> Simulation
addResources s newResources = s { resources=addRes (resources s) newResources }

numGeodes :: Simulation -> Int
numGeodes = flip getAvailableResource Geode . resources
--let res = resources s in getAvailableResource res Geode

runSimulation :: Simulation -> Simulation
runSimulation s@Simulation { timeRemaining=remaining}
    | trace (show ("time remaining:" ++ show remaining)) remaining == 0 = s
    | otherwise = maximumBy (compare `on` numGeodes) (map runSimulation (makeNextSimulations s canBeMadeRobots))
        where
            receivedResources = addRes (sumGenResources (rbts s)) (resources s)
            canBeMadeRobots = genActions (blueprint s) (trace ("received:" ++ show receivedResources) receivedResources)

makeNextSimulations :: Simulation -> [Robot] -> [Simulation]
-- can't make any robots, just add in the resources from the next minute and decrease the time remaining
makeNextSimulations s [] = [trace ("no new robots case" ++ show (resources s)) (time $ s { resources=sumGenResources (rbts s) })]
-- make simulations for the result of each choice we could have made
makeNextSimulations s newRobots = map (`addResources` newResources) nextSims
--makeNextSimulations s newRobots = map (flip addResources newResources) nextSimsWithRobots
    where
        nextSims = map (time . addRobotToSim s) newRobots
        --nextSimsWithTime = map time nextSimsWithRobots
        --nextSimsWithRobots = map (addRobotToSim s) newRobots
        newResources = sumGenResources (rbts s)

addRobotToSim :: Simulation -> Robot -> Simulation
addRobotToSim s newRobot@(Robot _ creationRequirements _) =
    s {
        rbts=newRobot:rbts s
        , resources=subResources (resources s) creationRequirements
    }

subResources :: Resources -> [CreationRequirement] -> Resources
subResources = foldr (flip subResource)
--the compiler told me that I didn't need to write all of this
--subResources res requirements =
    --foldr (\createReq prevRes -> subResource prevRes createReq) res requirements

subResource :: Resources -> CreationRequirement -> Resources
subResource res (CreationRequirement (ReqType Ore) number) = res { oreRes=oreRes res - number }
subResource res (CreationRequirement (ReqType Clay) number) = res { clayRes=clayRes res - number }
subResource res (CreationRequirement (ReqType Obsidian) number) = res { obsRes=obsRes res - number }
subResource _ (CreationRequirement (ReqType Geode) _) = undefined -- geodes can't be used to make anything

sumGenResources :: [Robot] -> Resources
sumGenResources currRobots =
    let madeRes = genResources currRobots in
        foldl addRes emptyResources madeRes

genResources :: [Robot] -> [Resources]
genResources = map getRes

genActions :: Blueprint -> Resources -> [Robot]
genActions (BluePrint { robots=checkRobots }) res = filter (canAffordRobot res) checkRobots

canAffordRobot :: Resources -> Robot -> Bool
canAffordRobot res (Robot _ creationReqs _) =
    all (hasResources res) (trace ("creation requirement:" ++ show creationReqs) creationReqs)

hasResources :: Resources -> CreationRequirement -> Bool
hasResources res (CreationRequirement (ReqType needRes) numNeeded) =
    let availRes = getAvailableResource res needRes in
        numNeeded <= availRes

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
            let numBlueprints = 1
                numMinutes = 24
                simulations = map (\bp -> Simulation {
                    blueprint=bp
                    , rbts=[Robot (RobotType Ore) [] GiveOre]
                    , resources=emptyResources
                    , timeRemaining=numMinutes
                    }
                    ) blueprints
            --print blueprints
            mapM_ print (take numBlueprints (map runSimulation simulations))


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
    P.try parseOre >> return (ReqType Ore)
    , parseObs >> return (ReqType Obsidian)
    , parseClay >> return (ReqType Clay)
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
        P.try parseOre >> return (RobotType Ore, GiveOre)
        , parseClay >> return (RobotType Clay, GiveClay)
        , parseObs >> return (RobotType Obsidian, GiveObs)
        , parseGeode >> return (RobotType Geode, GiveGeodes)
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
