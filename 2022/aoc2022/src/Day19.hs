module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

--import Debug.Trace
import Control.Monad.ST
import Control.Monad
import Data.STRef

import Data.List (
    intercalate
    , maximumBy
    , find
    , foldl'
    )

import Data.Maybe

import Data.Function (
    on
    )

data Giveable = GiveOre | GiveClay | GiveObs | GiveGeodes deriving (Show, Eq)

data Resource = Ore | Clay | Obsidian | Geode
    deriving (Show, Eq)

newtype ReqType = ReqType Resource
    deriving (Show, Eq)

data CreationRequirement = CreationRequirement ReqType Int
    deriving (Eq)
instance Show CreationRequirement where
    show (CreationRequirement (ReqType reqType) num) = "(Req " ++ show reqType ++ " " ++ show num ++ ")"

newtype RobotType = RobotType Resource
    deriving (Show, Eq)

robotTypeToNumber :: RobotType -> Int
robotTypeToNumber (RobotType Ore) = 0
robotTypeToNumber (RobotType Clay) = 0
robotTypeToNumber (RobotType Obsidian) = 0
robotTypeToNumber (RobotType Geode) = 0

instance Ord RobotType where
    type1 <= type2 = robotTypeToNumber type1 <= robotTypeToNumber type2

data Resources = Resources {
    oreRes :: Int
    , clayRes :: Int
    , obsRes :: Int
    , geodeRes :: Int
} deriving (Eq)

instance Show Resources where
    show r =
        "Resources (" ++ resourceCounts ++ ")"
        where
            resourceCounts = unwords
                (map (\(res, resName) -> resName ++ ": " ++ show (getAvailableResource r res)) resPairs)
            resPairs = [(Ore, "ore"), (Clay, "clay"), (Obsidian, "obs"), (Geode, "geode")]

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
    deriving (Eq)

instance Ord Robot where
    (Robot rbtType _ _) <= (Robot rbtType2 _ _) = rbtType <= rbtType2

instance Show Robot where
    show (Robot (RobotType rType) reqs gives) =
        "(Robot type:"
        ++ show rType
        ++ " " ++ show reqs
        ++ " " ++ show gives
        ++ ")"

getRobotTypeWrapped :: Robot -> RobotType
getRobotTypeWrapped (Robot robotType _ _) = robotType

getRobotType :: Robot -> Resource
getRobotType (Robot (RobotType rType) _ _) = rType

makeOreRobot :: CreationRequirement -> Robot
makeOreRobot req = Robot (RobotType Ore) [req] GiveOre

data Simulation = Simulation {
    blueprint :: Blueprint
    , rbts :: [Robot]
    , resources :: Resources
    , timeRemaining :: Int
    }

instance Show Simulation where
    show s = intercalate "\n" [
        "=========== Simulation ============="
        , show (blueprint s)
        , "Robots (" ++ unwords (map (show . getRobotType) (rbts s)) ++ ")"
        , show (resources s)
        , "timeRemaining:" ++ show (timeRemaining s)
        , "=========== Simulation ============="
        ]

time :: Simulation -> Simulation
time s@(Simulation { timeRemaining=rTime }) = s { timeRemaining=rTime - 1 }

addResources :: Simulation -> Resources -> Simulation
addResources s@(Simulation { resources=prevResources }) newResources
    = s { resources=addRes prevResources newResources }

numGeodes :: Simulation -> Int
numGeodes = flip getAvailableResource Geode . resources
--let res = resources s in getAvailableResource res Geode

type RobotStrategy = [RobotType] -> [Maybe Robot] -> [Maybe Robot]

alwaysMakeGeode :: RobotStrategy
alwaysMakeGeode _ canMakeRobots = case find ((== Geode) . getRobotType) (catMaybes canMakeRobots) of
    --Just rbt@(Robot (RobotType Geode) _ _) -> trace " forced geode" [Just rbt]
    Just rbt@(Robot (RobotType Geode) _ _) -> [Just rbt]
    --_ -> trace ("num can make robots" ++ show (map (show . getRobotType) (catMaybes canMakeRobots))) canMakeRobots
    _ -> canMakeRobots

preferNewRobot :: RobotStrategy
preferNewRobot _ [Nothing] = [Nothing]
preferNewRobot alreadyHaveRobots canMakeRobots =
    case newRobotTypes of
        [] -> canMakeRobots
        newTypes -> map Just newTypes
    where
        newRobotTypes = filter (robotTypeNotExist . getRobotTypeWrapped) justRobots
        justRobots = catMaybes canMakeRobots
        robotTypeNotExist = flip notElem alreadyHaveRobots


runSimulation :: [RobotStrategy] -> Simulation -> Simulation
runSimulation strategies s@Simulation { timeRemaining=remaining}
    -- | trace (show ("time remaining:" ++ show remaining)) remaining == 0 = s
    | remaining == 0 = s
    -- | remaining == 0 = s
    | otherwise = maximumBy (compare `on` numGeodes) (map (runSimulation strategies) (makeNextSimulations s (applyRobotStrategies strategies (map getRobotTypeWrapped (rbts s)) canBeMadeRobots)))
        where
            --totalResources = addRes re (resources s)
            --resourcesAddedNextMinute = sumGenResources (rbts s)
            --startResources = trace ("resources:" ++ show (resources s)) resources s
            startResources = resources s
            --canBeMadeRobots = genActions (blueprint s) (trace ("total available resources:" ++ show startResources) startResources)
            canBeMadeRobots = genActions (blueprint s) startResources

applyRobotStrategies :: [RobotStrategy] -> [RobotType] -> [Maybe Robot] -> [Maybe Robot]
applyRobotStrategies strategies alreadyRobotTypes canMakeRobots =
    foldr (\nextStrat acc -> nextStrat acc) canMakeRobots strategiesGivenAlreadyRobots
    where
        strategiesGivenAlreadyRobots = map (\x -> x alreadyRobotTypes) strategies


makeNextSimulations :: Simulation -> [Maybe Robot] -> [Simulation]
-- can't make any robots, just add in the resources from the next minute and decrease the time remaining
makeNextSimulations s newRobots
    | null (catMaybes newRobots) = [time $ s { resources=resources s `addRes` newResources }]
    | otherwise = nextSims
        -- make simulations for the result of each choice we could have made
        --makeNextSimulations s newRobots = nextSims
        --makeNextSimulations s newRobots = map (flip addResources newResources) nextSimsWithRobots
            where
                --nextSims = map ((`addResources` (trace ("new resources:" ++ show newResources) newResources)) . time . addRobotToSim s) newRobots
                nextSims = map ((`addResources` newResources) . time . addRobotToSim s) newRobots
                --nextSimsWithTime = map time nextSimsWithRobots
                --nextSimsWithRobots = map (addRobotToSim s) newRobots
                newResources = sumGenResources (rbts s)

addRobotToSim :: Simulation -> Maybe Robot -> Simulation
addRobotToSim s Nothing = s
addRobotToSim s (Just newRobot@(Robot _ creationRequirements _)) =
    s {
        --rbts=trace ("made new robot" ++ show newRobot) newRobot:rbts s
        rbts=newRobot:rbts s
        , resources=resourcesAfter
    }
    where
        resourcesAfter = subResources (resources s) creationRequirements

subResources :: Resources -> [CreationRequirement] -> Resources
subResources = foldl' subResource
--the compiler told me that I didn't need to write all of this
--subResources res requirements =
    --foldr (\createReq prevRes -> subResource prevRes createReq) res requirements

subResource :: Resources -> CreationRequirement -> Resources
subResource res (CreationRequirement (ReqType Ore) number) = res { oreRes=oreRes res - number }
subResource res (CreationRequirement (ReqType Clay) number) = res { clayRes=clayRes res - number }
subResource res (CreationRequirement (ReqType Obsidian) number) = res { obsRes=obsRes res - number }
subResource _ (CreationRequirement (ReqType Geode) _) = undefined -- geodes can't be used to make anything

--sumGenResources :: [Robot] -> Resources
--sumGenResources makeResRobots = runST $ do
--    newRes <- newSTRef emptyResources
--    forM_ makeResRobots $ \rbt -> do
--        modifySTRef' newRes $ addRes (getRes rbt)
--    readSTRef newRes

sumGenResources :: [Robot] -> Resources
sumGenResources =
    foldl' (\resAcc rbt ->  resAcc `addRes` getRes rbt) emptyResources

genResources :: [Robot] -> [Resources]
genResources = map getRes

genActions :: Blueprint -> Resources -> [Maybe Robot]
genActions (BluePrint { robots=checkRobots }) res = Nothing:map Just (filter (canAffordRobot res) checkRobots)

canAffordRobot :: Resources -> Robot -> Bool
canAffordRobot res (Robot _ creationReqs _) =
    --all (hasResources res) (trace ("creation requirement:" ++ show creationReqs) creationReqs)
    all (hasResources res) creationReqs

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
            let
                numBlueprints = 1
                --numBlueprints = length blueprints
                numMinutes = 24
                simulations = map (\bp -> Simulation {
                    blueprint=bp
                    , rbts=[Robot (RobotType Ore) [] GiveOre]
                    , resources=emptyResources
                    , timeRemaining=numMinutes
                    }
                    ) blueprints
            --print blueprints
                solvedSimulations = take numBlueprints (map (runSimulation strategies) simulations)
                answers = map (\s -> (qualityLevel s, bpId (blueprint s))) solvedSimulations
            mapM_ print answers
        where
            strategies = [preferNewRobot, alwaysMakeGeode]
            --strategies = [alwaysMakeGeode]

qualityLevel :: Simulation -> Int
qualityLevel s = let bp = blueprint s in bpId bp * getAvailableResource (resources s) Geode

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
