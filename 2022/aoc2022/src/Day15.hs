module Day15 where

import qualified Text.Parsec as P

import Data.Functor

import Debug.Trace

import qualified Data.Map as M

import Data.List (
    unfoldr
    , sort
    , find
    , groupBy
    )

import Data.Maybe (
    isJust
    )

import qualified Data.Set as S (
    fromList
    , difference
    , union
    , unions
    , Set
    , toList
    , fromList
    , intersection
    , empty
    , filter
    )

import System.IO.Strict as ST (
    readFile
    )

import Parsing (
    equals
    , digits
    , comma
    , plainWhitespace
    , integer
    )

-- 5525990, first try everytime
run :: String -> IO()
run inputFilePath = do
    let checkYCoord = 2 * ((10 :: Int) ^ (6 :: Int))
    print "day 15"
    fileContents <- ST.readFile inputFilePath
    case P.runParser parse () inputFilePath fileContents of
        Left parseError -> print parseError
        Right parseResults -> do
            --part1 parseResults checkYCoord
            part2 parseResults

part1 :: [Sensor] -> Int -> IO ()
part1 sensors targetY = do
    print "part1"
    let inRangeOfSensor = calculateImpossiblePlacesForY targetY sensors
    let cannotBeBeacon = postProcessImpossiblePlaces inRangeOfSensor sensors
    --print $ "checking y coord:" ++ show checkYCoord
    ----print $ sort cannotBeBeacon
    print $ "num impossible places:" ++ show (length cannotBeBeacon)

--part2 :: [Sensor] -> IO ()
--part2 sensors = do
--    print "part2"
--    print $ "num sensors:" ++ show (length sensors)
--    let sensorsWithDistances = getDistancesBetweenSensors sensors
--    let sensorsNotTouching = getSensorsNotTouching sensorsWithDistances
--    print $ "not touching:" ++ show sensorsNotTouching

part2 :: [Sensor] -> IO ()
part2 sensors = do
    print "part2"
    print $ "num sensors:" ++ show (length sensors)
    let gridDim = targetBeaconMaxCoord
    --let gridDim = 20
    print $ "grid dim:" ++ show gridDim
    let freeSpaces = findFreeSpaces gridDim gridDim sensors
    print $ "free spaces:" ++ show freeSpaces


findFreeSpaces :: Int -> Int -> [Sensor] -> [Coord]
findFreeSpaces gridX gridY sensors = go [] 0
    where
        beacons :: M.Map Coord ()
        beacons = M.fromList (map (\sens -> (beaconLoc sens, ())) sensors)
        getIter = iterationToCoord gridX gridY
        maxIteration = gridY * gridX
        go :: [Coord] -> Int -> [Coord]
        go acc currIdx
            | currIdx > maxIteration = acc
            | otherwise = let currCoord = getIter currIdx in
                --if or (map (inAreaOfSensor currCoord) sensors)
                if isJust (M.lookup currCoord beacons)
                then go acc (currIdx + 1)
                else case getFirstProhibitSensor currCoord sensors of
                    Nothing -> go (currCoord:acc) (currIdx + 1)
                    Just (xDiff, _) ->
                        if xDiff > 0
                        then go acc (currIdx + (trace ("able to skip" ++ show (xDiff * 2)) (xDiff * 2)))
                        else go acc (currIdx + 1)

                    --case find (inAreaOfSensor currCoord) sensors of
                    --    Nothing -> go (currCoord:acc) (currIdx + 1)
                    --    Just prohibitSensor -> []
                    --if any (inAreaOfSensor currCoord) sensors || 
                    --then go acc (currIdx + 1)
                    --else go (currCoord:acc) (currIdx + 1)

getFirstProhibitSensor :: Coord -> [Sensor] -> Maybe (Int, Sensor)
getFirstProhibitSensor c [] = Nothing
getFirstProhibitSensor c (s:ss) =
    let (isInArea, xDiff) = inAreaOfSensor c s in
        if isInArea then Just (xDiff, s)
        else getFirstProhibitSensor c ss


-- Manhatten Distance is the sum of the absolute difference between the
-- measures in all dimensions of two points
-- only doing 2D: x and y
-- returns (difference x (pos or neg), difference y (same), manhatten distance)
getDist :: Coord -> Coord -> (Int, Int, Int)
getDist (x1, y1) (x2, y2) =
    let
        xDiff = x2 - x1
        xDiffAbs = abs (x2 - x1)
        yDiff = y2 - y1
        yDiffAbs = abs yDiff
    in
        --xDiff `seq` yDiff `seq` (xDiff + yDiff)
        (xDiff, yDiff, xDiffAbs + yDiffAbs)

iterationToCoord :: Int -> Int -> Int -> Coord
iterationToCoord limitX limitY curr = (curr `mod` limitX,  curr `divRoundDown` limitY)

divRoundDown :: Int -> Int -> Int
n1 `divRoundDown` n2 = floor ((asNum1 / asNum2) :: Double)
    where
        asNum1 = fromIntegral n1
        asNum2 = fromIntegral n2

inAreaOfSensor :: Coord -> Sensor -> (Bool, Int)
inAreaOfSensor coord (Sensor sensCoord _ dist) =
    let (xDiff, _, distTo) = getDist coord sensCoord in
        (distTo <= dist, xDiff)


type SensorsDist = [(Sensor, [(Int, Sensor)])]

getSensorsNotTouching :: SensorsDist -> SensorsDist
getSensorsNotTouching = map notTouching

notTouching :: (Sensor, [(Int, Sensor)]) -> (Sensor, [(Int, Sensor)])
notTouching (sensor@(Sensor _ _ distToBeacon), neighbors) = (sensor, tooFarNeighbors)
    where
        tooFarNeighbors =
            filter (\(distTo, otherSensor) -> distTo > max distToBeacon (bDist otherSensor)) neighbors

getDistancesBetweenSensors :: [Sensor] -> SensorsDist
getDistancesBetweenSensors sensors =
    [ (s1, [(sensorDist s1 s2, s2) | s2 <- sensors, s2 /= s1]) | s1 <- sensors]

data Test = Test String [Coord]

sensorDist :: Sensor -> Sensor -> Int
sensorDist s1 s2 = let (_, _, dist) = getDist (loc s1) (loc s2) in dist

--test :: IO ()
--test = do
--    let tests = [
--            Test "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
--                [
--                (14, 3), (13, 3), (15, 3)
--                , (14, 2), (14, 4)
--                ]
--            , Test "Sensor at x=14, y=3: closest beacon is at x=16, y=3"
--                [
--                (14, 1), (14, 2), (14, 3), (14, 4), (14, 5)
--                , (13, 2), (13, 3), (13, 4)
--                , (12, 3)
--                , (15, 2), (15, 3), (15, 4)
--                , (16, 3)
--                ]
--            ]
--    testResults <- mapM runTest tests
--    mapM_ print testResults

--runTest :: Test -> IO (String, Bool, S.Set Coord, S.Set Coord)
runTest (Test input answers) = do
    case P.runParser parseLine () "" input of
        Left e -> print ("couldn't run test:" ++ input ++ " " ++ show e) >> return (input, False, S.empty, S.empty)
        Right sensor -> return (input, null diff, diff, results)
            where
                diff = S.difference (S.fromList answers) results
                results = getProhibitedSpacesAroundSensor sensor

findSensorForSpace :: [Sensor] -> Coord -> Maybe Sensor
findSensorForSpace sensors target = find (hasSpace target) sensors
    where
        hasSpace :: Coord -> Sensor -> Bool
        hasSpace (cx, cy) s = isJust $ find (\(x, y) -> x == cx && y == cy) (getProhibitedSpacesAroundSensor s)

--getGrid :: Int -> Int -> S.Set Coord
--getGrid x y = S.fromList (concat [ ([(cx, cy) | cx <- [0..x]]) | cy <- [0..y] ])

targetBeaconMaxCoord :: Int
targetBeaconMaxCoord = 4 * ((10 :: Int) ^ (6 :: Int))

tuningFrequency :: Int -> Int -> Int
tuningFrequency x y = x * targetBeaconMaxCoord + y

-- if the coordinate happened to be a beacon, it needs removed from
-- the list of spaces that couldn't possibly be a beacon
postProcessImpossiblePlaces :: [Coord] -> [Sensor] -> [Coord]
postProcessImpossiblePlaces iL ss =
    S.toList $ S.difference impossibleLocations sensorBeaconLocations
    where
        impossibleLocations = S.fromList iL
        sensorBeaconLocations = S.fromList (map beaconLoc ss)

type SensorLocation = Coord
type BeaconLocation = Coord
data Sensor = Sensor SensorLocation BeaconLocation Int deriving (Show, Ord, Eq)

loc :: Sensor -> SensorLocation
loc (Sensor sens _ _) = sens

beaconLoc :: Sensor -> BeaconLocation
beaconLoc (Sensor _ beac _) = beac

bDist :: Sensor -> Int
bDist (Sensor _ _ distToBeacon) = distToBeacon

type Coord = (Int, Int)

calculateImpossiblePlacesForY :: Int -> [Sensor] -> [Coord]
calculateImpossiblePlacesForY _ [] = []
calculateImpossiblePlacesForY y sensors = go [] (filter (isCloseTo y) sensors)
    where
    go :: [Coord] -> [Sensor] -> [Coord]
    go prohibited [] = prohibited
    go prohibited (s:ss) = go (prohibited++getProhibitedSpacesAtY y s) ss

getProhibitedSpacesAroundSensor :: Sensor -> S.Set Coord
getProhibitedSpacesAroundSensor sens@(Sensor (sx, sy) _ dist) =
    --let upProhibited = walkItBack (trace (show furthestUp) furthestUp) countDown stopUp
    --    downProhibited = walkItBack (trace (show furthestDown) furthestDown) countUp stopDown
    let upProhibited = walkItBack furthestUp countDown stopUp
        downProhibited = walkItBack furthestDown countUp stopDown
    in
        let total = S.union (S.fromList upProhibited) (S.fromList downProhibited) in
        trace ("finished sensor " ++ show sens) total

    where
        furthestUp = (sx, sy - dist)
        furthestDown = (sx, sy + dist)
        stopDown (_, y) = y == sy
        countDown (x, y) = (x + 1, y + 1)
        stopUp (_, y) = y == sy + 1
        countUp (x, y) = (x + 1, y - 1)

walkItBack :: Coord -> (Coord -> Coord) -> (Coord -> Bool) -> [Coord]
walkItBack start update stop  = go [] 0 start
    where
        go :: [Coord] -> Int -> Coord -> [Coord]
        go acc iter currCoord@(cx, cy) =
            if stop currCoord
            then acc
            else go (acc ++ newProhibited) (iter + 2) (update currCoord)
            where
                --newProhibited = [ (x, cy) | x <- [1..iter] ]
                newProhibited = foldl (\newPAcc newX -> (cx - newX, cy):newPAcc) [] [0..iter] -- [ (x, cy) | x <- [1..iter] ]

-- this assumes that the Y coordinate is in range of the sensor
getProhibitedSpacesAtY :: Int -> Sensor -> [Coord]
getProhibitedSpacesAtY y sens@(Sensor (sx, sy) _ dist) = leftList ++ rightList
    where
        leftList = unfoldr (\b -> goLeft b (b, y) (sx, sy) dist)  startX
        --leftList = unfoldr (\b -> if getDist (b, y) (sx, sy) <= dist then Just ((b, y), b - 1) else Nothing) startX
        rightList = unfoldr(\b -> goRight b (b, y) (sx, sy) dist) startX
        startX = sx
        goLeft = testOutFrom (\x -> x - 1)
        goRight = testOutFrom (+ 1)

testOutFrom :: (Int -> Int) -> Int -> Coord -> SensorLocation -> Int -> Maybe ((Int, Int), Int)
testOutFrom update currX loc1@(x1, y1) loc2 distToBeacon
    | distToSensor <= distToBeacon = Just ((x1, y1), update currX)
    | otherwise = Nothing
        where
            (_, _, distToSensor) = getDist loc1 loc2

isCloseTo :: Int -> Sensor -> Bool
isCloseTo checkY (Sensor (_, sy) _ dist) =
        abs (checkY - sy) <= dist

parseAssignment :: P.Parsec String () Int
--parseAssignment = (P.char 'x' P.<|> P.char 'y') *> equals *>
parseAssignment = do
    (P.char 'x' P.<|> P.char 'y') >> P.char '=' >> integer


parseCoords :: P.Parsec String () (Int, Int)
parseCoords = do
    x <- parseAssignment
    _ <- comma
    _ <-plainWhitespace
    y <- parseAssignment
    return (x, y)

parseBeacon :: P.Parsec String () ()
parseBeacon = P.string ": closest beacon is at " $> ()

parseSensor :: P.Parsec String () ()
parseSensor = P.string "Sensor at " $> ()

parseLine :: P.Parsec String () Sensor
parseLine = do
    _ <- parseSensor
    sensorCoords <- parseCoords
    _ <- parseBeacon
    beaconCoords <- parseCoords
    let (_, _, dist) = getDist sensorCoords beaconCoords
    return $ Sensor sensorCoords beaconCoords dist
    --return $ Sensor sensorCoords beaconCoords

parse :: P.Parsec String () [Sensor]
parse = P.many (parseLine <* P.endOfLine) <* P.eof
