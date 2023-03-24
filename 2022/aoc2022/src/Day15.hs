module Day15 where

import qualified Text.Parsec as P

import Data.Functor

import Debug.Trace

import Data.List (
    unfoldr
    , sort
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
            --let inRangeOfSensor = calculateImpossiblePlacesForY checkYCoord parseResults
            --let cannotBeBeacon = postProcessImpossiblePlaces inRangeOfSensor parseResults
            --print $ "checking y coord:" ++ show checkYCoord
            ----print $ sort cannotBeBeacon
            --print $ "num impossible places:" ++ show (length cannotBeBeacon)
            print "part2"
            --let gridDim = targetBeaconMaxCoord
            let gridDim = 20
            print $ "maxCoord:" ++ show gridDim
            let prohibitedSpaces = S.unions $ map getProhibitedSpacesAroundSensor parseResults
            let grid = getGrid gridDim gridDim
            --print $ "grid:" ++ show grid
            print $ "prohibitedSpaces:" ++ show prohibitedSpaces
            --print $ "difference:" ++ show (S.difference grid prohibitedSpaces)

data Test = Test String [Coord]

test :: IO ()
test = do
    let tests = [
            Test "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
                [
                    (14, 3), (13, 3), (15, 3)
                    , (14, 2), (14, 4)
                    ]
            ]
    testResults <- mapM runTest tests
    mapM_ print testResults

runTest :: Test -> IO (String, Bool, S.Set Coord, S.Set Coord)
runTest (Test input answers) = do
    case P.runParser parseLine () "" input of
        Left e -> print ("couldn't run test:" ++ input ++ " " ++ show e) >> return (input, False, S.empty, S.empty)
        Right sensor -> return (input, null diff, diff, results)
            where
                diff = S.difference (S.fromList answers) results
                results = getProhibitedSpacesAroundSensor sensor


getGrid :: Int -> Int -> S.Set Coord
getGrid x y = S.fromList (concat [ ([(cx, cy) | cx <- [0..x]]) | cy <- [0..y] ])

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
data Sensor = Sensor SensorLocation BeaconLocation Int deriving Show

loc :: Sensor -> SensorLocation
loc (Sensor sens _ _) = sens

beaconLoc :: Sensor -> BeaconLocation
beaconLoc (Sensor _ beac _) = beac

type Coord = (Int, Int)

calculateImpossiblePlacesForY :: Int -> [Sensor] -> [Coord]
calculateImpossiblePlacesForY _ [] = []
calculateImpossiblePlacesForY y sensors = go [] (filter (isCloseTo y) sensors)
    where
    go :: [Coord] -> [Sensor] -> [Coord]
    go prohibited [] = prohibited
    go prohibited (s:ss) = go (prohibited++getProhibitedSpacesAtY y s) ss

getProhibitedSpacesAroundSensor :: Sensor -> S.Set Coord
getProhibitedSpacesAroundSensor (Sensor (sx, sy) _ dist) =
    let upProhibited = walkItBack (trace (show furthestUp) furthestUp) countDown stopUp
        downProhibited = walkItBack (trace (show furthestDown) furthestDown) countUp stopDown
    in
        S.union (S.fromList upProhibited) (S.fromList downProhibited)
    where
        furthestUp = (sx, sy - dist)
        furthestDown = (sx, sy + dist)
        stopDown (_, y) = y == sy - 1
        countDown (x, y) = (x + 1, y + 1)
        stopUp (_, y) = y == sy
        countUp (x, y) = (x + 1, y - 1)

walkItBack :: Coord -> (Coord -> Coord) -> (Coord -> Bool) -> [Coord]
walkItBack start update stop  = go [start] 1 start
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
            distToSensor = getDist loc1 loc2

isCloseTo :: Int -> Sensor -> Bool
isCloseTo checkY (Sensor (_, sy) _ dist) =
        abs (checkY - sy) <= dist

-- Manhatten Distance is the sum of the absolute difference between the
-- measures in all dimensions of two points
-- only doing 2D: x and y
getDist :: Coord -> Coord -> Int
getDist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

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
    return $ Sensor sensorCoords beaconCoords (getDist sensorCoords beaconCoords)
    --return $ Sensor sensorCoords beaconCoords

parse :: P.Parsec String () [Sensor]
parse = P.many (parseLine <* P.endOfLine) <* P.eof
