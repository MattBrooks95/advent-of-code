{-# LANGUAGE BangPatterns #-}
module Day19 where

import qualified Text.Parsec as P
import qualified Parsing as PS

import System.Exit (
    die
    )

import Control.Monad (
    when
    )

import Debug.Trace

import qualified Data.Set as S
import Data.List (
    intercalate
    , maximumBy
    , find
    , foldl'
    , minimumBy
    )

import qualified Data.Map as M
import Data.Maybe

import Data.Function (
    on
    )

myTrace :: String -> a -> a
--myTrace = trace
myTrace _ a = a


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
robotTypeToNumber (RobotType Clay) = 1
robotTypeToNumber (RobotType Obsidian) = 2
robotTypeToNumber (RobotType Geode) = 3

instance Ord RobotType where
    type1 <= type2 = robotTypeToNumber type1 <= robotTypeToNumber type2

data Resources = Resources {
    oreRes :: Int
    , clayRes :: Int
    , obsRes :: Int
    , geodeRes :: Int
} deriving (Eq)

emptyRes :: Resources
emptyRes = Resources {
    oreRes=0
    , clayRes=0
    , obsRes=0
    , geodeRes=0
    }

resourcesScalar :: Int -> Resources -> Resources
resourcesScalar scalar res = Resources {
    oreRes=oreRes res * scalar
    , clayRes=clayRes res * scalar
    , obsRes=obsRes res * scalar
    , geodeRes=geodeRes res * scalar
    }

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
addRes res1@(Resources { oreRes=ore1, clayRes=clay1, obsRes=obs1, geodeRes=geode1 }) res2@(Resources { oreRes=ore2, clayRes=clay2, obsRes=obs2, geodeRes=geode2}) =
    res1 `seq` res2 `seq` Resources {
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

getRobotRequirements :: Robot -> [CreationRequirement]
getRobotRequirements (Robot _ reqs _) = reqs

makeOreRobot :: CreationRequirement -> Robot
makeOreRobot req = Robot (RobotType Ore) [req] GiveOre

data RobotCount a = RobotCount {
    rcOre :: !a
    , rcClay :: !a
    , rcObs :: !a
    , rcGeode :: !a
    } deriving (Show, Eq)

emptyRobotReqCount :: RobotCount [CreationRequirement]
emptyRobotReqCount = RobotCount {
    rcOre=[]
    , rcClay=[]
    , rcObs=[]
    , rcGeode=[]
    }

emptyRobotCount :: RobotCount Int
emptyRobotCount = RobotCount {
    rcOre=0
    , rcClay=0
    , rcObs=0
    , rcGeode=0
    }

getUniqueRobotTypes :: RobotCount Int -> Int
getUniqueRobotTypes (RobotCount { rcOre=rco, rcClay=rcc, rcObs=rcobs, rcGeode=rcg }) =
    length $ filter (/= 0) [rco, rcc, rcobs, rcg]

addRobot :: RobotType -> RobotCount Int -> RobotCount Int
addRobot (RobotType Ore) rc@(RobotCount { rcOre=ore }) = rc { rcOre=ore + 1 }
addRobot (RobotType Clay) rc@(RobotCount { rcClay=clay }) = rc { rcClay=clay + 1 }
addRobot (RobotType Obsidian) rc@(RobotCount { rcObs=obsidian }) = rc { rcObs=obsidian + 1 }
addRobot (RobotType Geode) rc@(RobotCount { rcGeode=geode }) = rc { rcGeode=geode + 1 }

data Simulation = Simulation {
    blueprint :: !Blueprint
    , rbts :: !(RobotCount Int)
    , resources :: !Resources
    , timeRemaining :: !Int
    } deriving (Eq)

getBlankSim :: Simulation
getBlankSim = Simulation {
    blueprint=emptyBp
    , resources=emptyRes
    , rbts=emptyRobotCount
    , timeRemaining=0
    }

getSimulationResource :: Simulation -> Resource -> Int
getSimulationResource sim = getAvailableResource (resources sim)

instance Show Simulation where
    show s = intercalate "\n" [
        "=========== Simulation ============="
        , show (blueprint s)
        , show (rbts s)
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

type RobotStrategy = [RobotType] -> [Maybe Robot] -> [Maybe Robot]

type SimulationsByTime = M.Map Int [Simulation]

runSimulation :: (Simulation, S.Set RobotType) -> Simulation
runSimulation (startS@(Simulation { timeRemaining=rTime }), cantMakeRobots) = myTrace (show (timeRemaining startS) ++ ": minutes remaining, prohibited robots:" ++ show cantMakeRobots) $
    if rTime > 0
    then
        case toNextDecision startS of
            -- we have no decision to make here, skip ahead to the next decision
            --Left nextSim -> startS:runSimulation (nextSim, cantMakeRobots)
            -- this method saves the skipped states for debugging
            Left nextSims -> runSimulation (last nextSims, cantMakeRobots)
            -- we can make a robot this turn, we must decide which robot if any to make
            --
            -- apply the optimization you read about online where refusing to make a robot
            -- at a given time T means that you will not make that robot again until you make a robot
            -- of a different type. The reason is, that if you were going to make that robot anyway
            -- you should have made it as soon as possible
            Right canMakeRobots -> maximumBy (compare `on` flip getSimulationResource Geode) (map runSimulation nextSims)
                where
                    nextSims = makeNextSimulations (startS, cantMakeRobots) notProhibitedRobots
                    notProhibitedRobots = filter (\r -> getRobotTypeWrapped r `notElem` cantMakeRobots) canMakeRobots
    else startS

--newtype CompareSimulation = CompareSimulation Simulation
--instance Eq CompareSimulation where
--    (CompareSimulation sim1) == (CompareSimulation sim2) = sim1 == sim2
--instance Ord CompareSimulation where
--    (CompareSimulation sim1) <= (CompareSimulation sim2) =
--        let robotTypesCountSim1 = getRobotTypeCount sim1
--            robotTypesCountSim2 = getRobotTypeCount sim2
--        in
--            if robotTypesCountSim1 == robotTypesCountSim2
--            then getGeodeResourceCount sim1 <= getGeodeResourceCount sim2
--            else robotTypesCountSim1 <= robotTypesCountSim2
--        where
--            getGeodeResourceCount = flip getSimulationResource Geode
--            getRobotTypeCount = getUniqueRobotTypes . rbts

toNextDecision :: Simulation -> Either [Simulation] [Robot]
toNextDecision sim@(Simulation { blueprint=bp, resources=res, rbts=rc }) =
    let canMakeRobots = genActions bp res
    in
        if null canMakeRobots
        then

            let (_, soonestRobotTime) = soonestRobot $ timeToRobots bp res rc in
                myTrace
                    ("advanced to next decision, skipping:" ++ show soonestRobotTime ++ "minutes")
                    (Left [sim, advance sim soonestRobotTime])
                    --(Left $ advanceWithIntermediates sim soonestRobotTime)
        else myTrace ("can make robots this turn:" ++ show (map getRobotType canMakeRobots)) (Right canMakeRobots)

advance :: Simulation -> Int -> Simulation
advance startSim numMinutes =
    startSim {
        resources=nextResources
        , timeRemaining=nextTime
    }
    where
        nextTime=rTime - actualSkippedMinutes
        nextResources = let newRes = resourcesScalar actualSkippedMinutes resourcesPerMinute in addRes (resources startSim) newRes
        resourcesPerMinute = sumGenResources (rbts startSim)
        actualSkippedMinutes=let timeDiff = rTime - numMinutes in numMinutes - (if timeDiff < 0 then abs timeDiff else 0)
        rTime=timeRemaining startSim

advanceWithIntermediates :: Simulation -> Int -> [Simulation]
advanceWithIntermediates startSim numMinutes = scanl (\prev _ -> advance prev 1) startSim [1..numMinutes]

-- this RobotCount parameter needs to be a map of robot type to the number of turns
-- it will take to make that robot
soonestRobot :: RobotCount (Maybe Int) -> (RobotType, Int)
soonestRobot (RobotCount { rcOre=rco, rcClay=rcc, rcObs=rcob, rcGeode=rcg }) =
    minimumBy (compare `on` snd) (catMaybes [
        reqOrNothing (RobotType Ore, rco)
        , reqOrNothing (RobotType Clay, rcc)
        , reqOrNothing (RobotType Obsidian, rcob)
        , reqOrNothing (RobotType Geode, rcg)
        ])
    where
        reqOrNothing :: (RobotType, Maybe Int) -> Maybe (RobotType, Int)
        reqOrNothing (_, Nothing) = Nothing
        reqOrNothing (rbtType, Just numTurns) = Just (rbtType, numTurns)

--this returns a robot count type, but the meaning of the integer
--is the number of minutes it would take to become able to build that robot
timeToRobots :: Blueprint -> Resources -> RobotCount Int -> RobotCount (Maybe Int)
timeToRobots bp res rc = countTurnsToSatisfyReq res rc reqMap
    --RobotCount {
    --    --rcOre = maximum $ map countTurns oreRobotReq
    --    --rcOre = foldl' (\prev req -> max prev (countTurns req)) 0 oreRobotReq
    --    rcOre = findLongestRec oreRobotReq
    --    , rcClay = findLongestRec clayRobotReq
    --    , rcObs = findLongestRec obsRobotReq
    --    , rcGeode = findLongestRec geodeRobotReq
    --    }
    where
        --findLongestRec = foldl' (\prev req -> let turns = countTurns req in max prev turns) 0
        --findLongestRec req = maximum $ map countTurns req
        reqMap = RobotCount {
            rcOre=getRobotRequirements (bpOreRobot bp)
            , rcClay=getRobotRequirements (bpClayRobot bp)
            , rcObs=getRobotRequirements (bpObsRobot bp)
            , rcGeode=getRobotRequirements (bpGeodeRobot bp)
            }
        --oreRobotReq = getRobotRequirements (bpOreRobot bp)
        --clayRobotReq = getRobotRequirements (bpClayRobot bp)
        --obsRobotReq = getRobotRequirements (bpObsRobot bp)
        --geodeRobotReq = getRobotRequirements (bpGeodeRobot bp)
        --countTurns = countTurnsToSatisfyReq res rc

countTurnsToSatisfyReq :: Resources -> RobotCount Int -> RobotCount [CreationRequirement] -> RobotCount (Maybe Int)
countTurnsToSatisfyReq res rc (RobotCount {rcOre=oreReqs, rcClay=clayReqs, rcObs=obsReqs, rcGeode=geodeReqs }) =
    RobotCount {
        rcOre=handleNoReq oreReqs (maximum $ map (\(CreationRequirement _ resNum) -> getTurnsTo (getAvailableResource res Ore) resNum (rcOre rc)) oreReqs)
        , rcClay=handleNoReq clayReqs (maximum $ map (\(CreationRequirement _ resNum) -> getTurnsTo (getAvailableResource res Clay) resNum (rcClay rc)) clayReqs)
        , rcObs=handleNoReq obsReqs (maximum $ map (\(CreationRequirement _ resNum) -> getTurnsTo (getAvailableResource res Obsidian) resNum (rcObs rc)) obsReqs)
        , rcGeode=handleNoReq geodeReqs (maximum $ map (\(CreationRequirement _ resNum) -> getTurnsTo (getAvailableResource res Geode) resNum (rcGeode rc)) geodeReqs)
        }
        where
            handleNoReq :: [CreationRequirement] -> Int -> Maybe Int
            handleNoReq [] _ = Nothing
            handleNoReq reqs answer
                | myTrace ("countTurnsToSatisfyReq, availRes:" ++ show res ++ " reqs:" ++ show reqs ++ " answer:" ++ show answer ) answer <= 0  = Nothing
                | otherwise = Just answer
--countTurnsToSatisfyReq res rc (CreationRequirement (ReqType Ore) resNum) = ceiling $ (fromIntegral (resNum - getAvailableResource res Ore) :: Double) / fromIntegral (rcOre rc)
--countTurnsToSatisfyReq res rc (CreationRequirement (ReqType Clay) resNum) = ceiling $ (fromIntegral (resNum - getAvailableResource res Clay) :: Double) / fromIntegral (rcClay rc)
--countTurnsToSatisfyReq res rc (CreationRequirement (ReqType Obsidian) resNum) = ceiling $ (fromIntegral (resNum - getAvailableResource res Obsidian) :: Double) / fromIntegral (rcObs rc)
--countTurnsToSatisfyReq res rc (CreationRequirement (ReqType Geode) resNum) = ceiling $ (fromIntegral (resNum - getAvailableResource res Geode) :: Double) / fromIntegral (rcGeode rc)

-- | # available resources -> # needed resources -> number of robots generating resource
getTurnsTo :: Int -> Int -> Int -> Int
getTurnsTo hasRes needRes numGeneratingRobots =
    --resNum `seq` reqRes `seq` numGeneratingRobots `seq` if diff <= 0 then 0 else ceiling $ (fromIntegral diff :: Double) / fromIntegral numGeneratingRobots
    if neededRes <= 0 then 0 else ceiling $ (fromIntegral neededRes :: Double) / fromIntegral numGeneratingRobots
    where
        neededRes = needRes - hasRes

detectNewRobotType :: [RobotType] -> S.Set RobotType -> S.Set RobotType
detectNewRobotType newRobots = S.difference (S.fromList newRobots)

hasRobotType :: RobotType -> RobotCount Int -> Bool
hasRobotType (RobotType Ore) rc = rcOre rc > 0
hasRobotType (RobotType Clay) rc = rcClay rc > 0
hasRobotType (RobotType Obsidian) rc = rcObs rc > 0
hasRobotType (RobotType Geode) rc = rcGeode rc > 0

hasGeodeRobotSim :: Simulation -> Bool
hasGeodeRobotSim s = hasRobotType (RobotType Geode) (rbts s)

makeNextSimulations :: (Simulation, S.Set RobotType) -> [Robot] -> [(Simulation, S.Set RobotType)]
-- can't make any robots, just add in the resources from the next minute and decrease the time remaining
makeNextSimulations (s, cantMakeRobots) newRobots = nextSims
            where
                -- when you make a robot, reset the list of robots that can't be made
                -- if you intentionally don't make a set of robots, those robots cannot be made until you've made
                -- a robot of another type. This is a hint that I read online where it's true that because you
                -- can only make one robot a minute, it never makes sense to delay making a type of robot that you
                -- could have made earlier
                -- do not consider a route where all robots are forbidden, you should atleast make a geode robot
                -- note that this is bad design because I hard coded 4 to mean the # of robots to choose from,
                -- when that value should actually depend on the parsed blueprint
                nextSims = nextSimsMadeRobot ++ (if S.size newCantMakeRobotsList /= 4 then [nextSimsNoNewRobot] else [])
                nextSimsNoNewRobot = (nextStepSim newResources Nothing s, newCantMakeRobotsList)
                nextSimsMadeRobot = map (\makeRobot -> (nextStepSim newResources (Just makeRobot) s, S.empty)) newRobots
                newCantMakeRobotsList = S.union cantMakeRobots (S.fromList $ map getRobotTypeWrapped newRobots)
                newResources = sumGenResources (rbts s)

nextStepSim :: Resources -> Maybe Robot -> Simulation -> Simulation
nextStepSim newResources potentialRobot sim =
    let withResources = addResources sim newResources in
        (time . addRobotToSim potentialRobot) withResources

addRobotToSim :: Maybe Robot -> Simulation -> Simulation
addRobotToSim Nothing s = s
addRobotToSim (Just newRobot@(Robot _ creationRequirements _)) s =
    s {
        rbts=addRobot (getRobotTypeWrapped newRobot) (rbts s)
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

sumGenResources :: RobotCount Int -> Resources
sumGenResources rc = Resources {
    oreRes = rcOre rc
    , clayRes = rcClay rc
    , obsRes = rcObs rc
    , geodeRes = rcGeode rc
    }

genActions :: Blueprint -> Resources -> [Robot]
genActions bp res = filter (canAffordRobot res) (getBpRobots bp)

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
    , bpOreRobot :: Robot
    , bpClayRobot :: Robot
    , bpObsRobot :: Robot
    , bpGeodeRobot :: Robot
    } deriving (
        Eq
        )
emptyBp :: Blueprint
emptyBp = BluePrint {
    bpId=0
    , bpOreRobot=Robot (RobotType Ore) [] GiveOre
    , bpClayRobot=Robot (RobotType Clay) [] GiveClay
    , bpObsRobot=Robot (RobotType Obsidian) [] GiveObs
    , bpGeodeRobot=Robot (RobotType Geode) [] GiveGeodes
    }

getBpRobots :: Blueprint -> [Robot]
getBpRobots (BluePrint { bpOreRobot=orr, bpClayRobot=cr, bpObsRobot=obr, bpGeodeRobot=gr }) =
    [orr, cr, obr, gr]

instance Show Blueprint where
    show bp = "\n(BP " ++ show (bpId bp)
        ++ "\n\t" ++ intercalate "\n\t" (map show (getBpRobots bp))
        ++ "\n)"

run :: String -> IO ()
run input = do
    print "day19"
    case P.runParser parse () "" input of
        Left e -> print e
        Right blueprints -> do
            when (length (catMaybes blueprints) /= length blueprints) (die "failed to parse at least one blueprint")
            let
                numBlueprints = 2 :: Int
                --numBlueprints = length blueprints
                numMinutes = 24
                simulations = map (\bp -> (Simulation {
                        blueprint=bp
                        , rbts=RobotCount { rcOre=1, rcClay=0, rcObs=0, rcGeode=0 }
                        , resources=emptyResources
                        , timeRemaining=numMinutes
                        }
                        , S.empty
                        )
                    ) (catMaybes blueprints)
                solved = take numBlueprints (foldl' (\acc sim -> runSimulation sim:acc) [] simulations)
            --mapM_ print solved
            --
            --let withQualityLevels = map (\simSteps -> (simSteps, (qualityLevel . last) simSteps)) solved
            let withQualityLevels = map (\simSteps -> (simSteps, qualityLevel simSteps)) solved
                sumQualityLevel = sum (map snd withQualityLevels)
            print $ "sum quality level:" ++ show sumQualityLevel
            --print "quality levels:"
            --print withQualityLevels
            --let printLines = map (\(simList, ql) -> "(bpId:" ++ show (bpId (last simList)) ++ ", qualityLevel:" ++ show ql ++ ")") withQualityLevels
            --mapM_ putStrLn printLines
        --where
        --    bestSim :: [[Simulation]] -> [([Simulation], Int)]
        --    bestSim simSolutions =
        --        let withQualityLevels = map (\simSteps -> (simSteps, (qualityLevel . last) simSteps)) simSolutions
        --        in maximumBy (compare `on` snd) withQualityLevels

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
    additionalCosts <- P.many parseAdditionalCost
    _ <- P.char '.'
    return $ Robot robotType (firstCost:additionalCosts) giveable

parseBlueprint :: P.Parsec String () (Maybe Blueprint)
parseBlueprint = do
    _ <- P.string "Blueprint "
    thisBpId <- PS.digits
    _ <- P.string ":"
    thisRobots <- P.many parseRobot
    case robotListToBp thisBpId thisRobots of
        Nothing -> return Nothing
        Just bp -> return $ Just bp

robotListToBp :: Int -> [Robot] -> Maybe Blueprint
robotListToBp blueprintId robots = do
    oreRbt <- find (\r -> getRobotType r == Ore) robots
    clayRbt <- find (\r -> getRobotType r == Clay) robots
    obsRbt <- find (\r -> getRobotType r == Obsidian) robots
    geodeRbt <- find (\r -> getRobotType r == Geode) robots
    Just $ BluePrint {
        bpId=blueprintId
        , bpOreRobot=oreRbt
        , bpClayRobot=clayRbt
        , bpObsRobot=obsRbt
        , bpGeodeRobot=geodeRbt
        }

parse :: P.Parsec String () [Maybe Blueprint]
--still need to figure out why just P.sepBy doesn't work
--parse = (parseBlueprint `P.sepBy` P.endOfLine) <* P.eof
parse = P.many (parseBlueprint <* P.endOfLine) <* P.eof
