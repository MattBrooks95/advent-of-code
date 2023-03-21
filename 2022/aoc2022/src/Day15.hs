module Day15 where

import qualified Text.Parsec as P

import Data.Functor

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

run :: String -> IO()
run inputFilePath = do
    print "day 15"
    fileContents <- ST.readFile inputFilePath
    let parseResults = P.runParser parse () inputFilePath fileContents
    print parseResults

type SensorLocation = (Int, Int)
type BeaconLocation = (Int, Int)
data Sensor = Sensor SensorLocation BeaconLocation deriving Show

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
    Sensor sensorCoords <$> parseCoords
    --return $ Sensor sensorCoords beaconCoords

parse :: P.Parsec String () [Sensor]
parse = P.many (parseLine <* P.endOfLine) <* P.eof
