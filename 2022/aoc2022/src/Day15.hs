module Day15 where

import qualified Text.Parsec as P

import Data.Functor

import qualified Data.Set as S

import Data.List (
    unfoldr
    , sort
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
            let inRangeOfSensor = calculateImpossiblePlacesForY checkYCoord parseResults
            let cannotBeBeacon = postProcessImpossiblePlaces inRangeOfSensor parseResults
            print $ "checking y coord:" ++ show checkYCoord
            --print $ sort cannotBeBeacon
            print $ "num impossible places:" ++ show (length cannotBeBeacon)

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
