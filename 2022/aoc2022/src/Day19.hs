module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

import Debug.Trace
--import Control.Monad.ST
--import Control.Monad
--import Data.STRef

import qualified Data.Set as S
import Data.List (
    intercalate
    , maximumBy
    , find
    , foldl'
    )

import qualified Data.Map as M
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
    Just rbt@(Robot (RobotType Geode) _ _) -> [Just rbt]
    _ -> canMakeRobots

--preferNewRobot :: RobotStrategy
--preferNewRobot _ [Nothing] = [Nothing]
--preferNewRobot alreadyHaveRobots canMakeRobots =
--    case newRobotTypes of
--        [] -> canMakeRobots
--        newTypes -> map Just newTypes
--    where
--        newRobotTypes = filter (robotTypeNotExist . getRobotTypeWrapped) justRobots
--        justRobots = catMaybes canMakeRobots
--        robotTypeNotExist = flip notElem alreadyHaveRobots

type Firsts = S.Set (RobotType, Int)

type SimulationsByTime = M.Map Int [Simulation]

runSimulation :: [RobotStrategy] -> Simulation -> [Simulation]
runSimulation strategies startS = let (doneSims, _, _) = go ([], M.fromList [(0, [startS])], 0) in doneSims
    -- | remaining == 0 = s
    -- | remaining == 0 = s
    -- | otherwise = maximumBy (compare `on` numGeodes) (map (runSimulation strategies) (makeNextSimulations s (applyRobotStrategies strategies (map getRobotTypeWrapped (rbts s)) canBeMadeRobots)))
    --     where
    --         --totalResources = addRes re (resources s)
    --         --resourcesAddedNextMinute = sumGenResources (rbts s)
    --         startResources = resources s
    --         canBeMadeRobots = genActions (blueprint s) startResources
    where
        go :: ([Simulation], SimulationsByTime, Int) -> ([Simulation], SimulationsByTime, Int)
        go (doneSims, simsByTime, processTime)
            | M.null simsByTime = (doneSims, simsByTime, processTime)
            | processTime == 0 = (doneSims, simsByTime, processTime)
            | otherwise = case M.lookup processTime simsByTime of
                Nothing -> go (doneSims, simsByTime, processTime - 1)
                Just [] -> go (doneSims, simsByTime, processTime - 1)
                Just (processSim:remainingSims) ->
                    let rTime = timeRemaining processSim
                        currRobotTypes = map getRobotTypeWrapped (rbts processSim)
                        startResources = resources processSim
                        canBeMadeRobots = genActions (blueprint startS) startResources
                        canBeMadeRobotsAfterStrategies = applyRobotStrategies strategies currRobotTypes canBeMadeRobots
                    in
                    case timeRemaining (trace (show processSim) processSim) of
                --go (doneSims, processSim:remainingSims) = case timeRemaining processSim of
                        0 -> go (processSim:doneSims, M.insert processTime remainingSims simsByTime, processTime)
                        _ -> let nextSims = makeNextSimulations processSim canBeMadeRobotsAfterStrategies in
                            case M.lookup (processTime - 1) simsByTime of
                                Nothing -> go (doneSims, M.insert (processTime - 1) nextSims (M.insert processTime remainingSims simsByTime), processTime)
                                Just alreadyNextSims -> go (doneSims, M.insert (processTime - 1) (nextSims ++ alreadyNextSims) simsByTime, processTime)

detectNewRobotType :: [RobotType] -> S.Set RobotType -> S.Set RobotType
detectNewRobotType newRobots = S.difference (S.fromList newRobots)

hasRobotType :: RobotType -> [RobotType] -> Bool
robotType `hasRobotType` listOfRobotTypes = robotType `elem` listOfRobotTypes

hasGeodeRobotSim :: Simulation -> Bool
hasGeodeRobotSim s = let currRobotTypes = map getRobotTypeWrapped (rbts s) in
    hasRobotType (RobotType Geode) currRobotTypes

-- idea: when you make a geode robot for the first time, you can remove all simulations
-- with the same amount of time remaining that do not have/can not make a geode robot
purgeBadStrategies :: Firsts -> [Simulation] -> [Simulation]
purgeBadStrategies firsts sims = S.foldl' (\acc first -> filter (not . isBadSim first) acc) sims firsts

getRobotTypesOfSim :: Simulation -> S.Set RobotType
getRobotTypesOfSim (Simulation { rbts=thisSimRobots }) = S.fromList (map getRobotTypeWrapped thisSimRobots)

isBadSim :: (RobotType, Int) -> Simulation -> Bool
isBadSim (rType, timeFound) sim
    | timeRemaining sim >= timeFound = False
    | otherwise = let simTypes = getRobotTypesOfSim sim in rType `S.member` simTypes

applyRobotStrategies :: [RobotStrategy] -> [RobotType] -> [Maybe Robot] -> [Maybe Robot]
applyRobotStrategies strategies alreadyRobotTypes canMakeRobots =
    foldr (\nextStrat acc -> nextStrat acc) canMakeRobots strategiesGivenAlreadyRobots
    where
        strategiesGivenAlreadyRobots = map (\x -> x alreadyRobotTypes) strategies


makeNextSimulations :: Simulation -> [Maybe Robot] -> [Simulation]
-- can't make any robots, just add in the resources from the next minute and decrease the time remaining
makeNextSimulations s newRobots
    | null (catMaybes newRobots) = let newSim = time $ addResources s newResources in
        [newSim]
    | otherwise = nextSims
        -- make simulations for the result of each choice we could have made
        --makeNextSimulations s newRobots = nextSims
        --makeNextSimulations s newRobots = map (flip addResources newResources) nextSimsWithRobots
            where
                nextSims = map (\r -> nextStepSim newResources r s) newRobots
                --nextSims = map ((`addResources` newResources) . time . addRobotToSim s) newRobots
                --nextSimsWithTime = map time nextSimsWithRobots
                --nextSimsWithRobots = map (addRobotToSim s) newRobots
                newResources = sumGenResources (rbts s)

nextStepSim :: Resources -> Maybe Robot -> Simulation -> Simulation
nextStepSim newResources potentialRobot sim =
    let withResources = addResources sim newResources in
        (time . addRobotToSim potentialRobot) withResources

addRobotToSim :: Maybe Robot -> Simulation -> Simulation
addRobotToSim Nothing s = s
addRobotToSim (Just newRobot@(Robot _ creationRequirements _)) s =
    s {
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
                numMinutes = 10
                simulations = map (\bp -> Simulation {
                    blueprint=bp
                    , rbts=[Robot (RobotType Ore) [] GiveOre]
                    , resources=emptyResources
                    , timeRemaining=numMinutes
                    }
                    ) blueprints
            --print blueprints
                solvedSimulations = map (runSimulation strategies) simulations
                printSolutions = take numBlueprints solvedSimulations
                bestSolutions = map bestSim printSolutions
                answers = map (\s -> (qualityLevel s, bpId (blueprint s))) bestSolutions
            mapM_ print answers
            --mapM_ (print . length) solvedSimulations
        where
            bestSim :: [Simulation] -> Simulation
            bestSim = maximumBy (compare `on` numGeodes)
    -- | otherwise = maximumBy (compare `on` numGeodes) (map (runSimulation strategies) (makeNextSimulations s (applyRobotStrategies strategies (map getRobotTypeWrapped (rbts s)) canBeMadeRobots)))
            --strategies = []
            strategies = [alwaysMakeGeode]
            --strategies = [preferNewRobot, alwaysMakeGeode]

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
