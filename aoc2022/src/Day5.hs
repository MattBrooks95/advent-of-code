module Day5 (
    run
    , parseMove
    , getIntsFromMoveLine
    ) where

import qualified Data.Text as T (
    pack
    , splitOn
    , unpack
    , Text(..)
    )

import Data.Maybe (
    mapMaybe
    )

import Text.Regex.TDFA

import Debug.Trace (
    trace
    )

import Lib (
    groupLines
    )

data Stack = Stack {
    stackContents :: String
    , stackId :: Int
    } deriving (Show)

data Move = Move { from::Int, to::Int, number::Int } deriving (Show)

data CratesSimulation = CratesSimulation {
    stacks :: [Stack]
    , moves :: [Move]
    } deriving (Show)

run :: [String] -> IO ()
run inputLines = do
    let cratesSimulation = parseDay5 inputLines
    print "day5"
    print (stacks cratesSimulation)

parseDay5 :: [String] -> CratesSimulation
parseDay5 inputLines = CratesSimulation { stacks=parsedStacks, moves = parsedMoves }
    where
        (crateLines, stackIdLine, moveLines) = organizeLines inputLines
        parsedMoves = parseMoves moveLines
        parsedIdLine = parseIdLine stackIdLine
        parsedCrates = concatMap parseCrates crateLines
        parsedStacks = map (`buildStackFromCrates` parsedCrates) parsedIdLine

buildStackFromCrates :: Int -> [(Maybe Char, Int)] -> Stack
buildStackFromCrates parsedId potentialCrates = Stack { stackId=parsedId, stackContents=justCrates}
    where
        justCrates = mapMaybe fst cratesForStackId
        cratesForStackId = filter (\(_, sid) -> sid == parsedId) potentialCrates



organizeLines :: [String] -> ([String], String, [String])
organizeLines [] = ([], "", [])
organizeLines input = (crateLines, stackIdLine, moveLines)
    where
        --splitOnEmptyLine = trace (show $ groupLines input) (groupLines input)
        splitOnEmptyLine = groupLines input
        crateLines = init topHalf
        stackIdLine = last topHalf
        topHalf = head splitOnEmptyLine
        moveLines = last splitOnEmptyLine

parseCrate :: String -> Maybe Char
parseCrate input = if length input > 3 && secondElem /= ' ' then
    Just secondElem
    else Nothing
    where
        secondElem = input !! 1

-- parse a line of crates,
-- assigning them an id in 1..# of crates
-- so that afterwards we can rearrange them into stacks based on
-- their id
parseCrates :: String -> [(Maybe Char, Int)]
parseCrates input = reverse $ parseCrates' [] 1 input

parseCrates' :: [(Maybe Char, Int)] -> Int -> String -> [(Maybe Char, Int)]
parseCrates' acc _ [] = acc
parseCrates' acc counter input = parseCrates' (newCrate:acc) (counter + 1) (drop 4 input)
    where
        newCrate = (parseCrate input, counter)

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseMove :: String -> Move
parseMove input = Move { from=parsedFrom, to=parsedTo, number=parsedNumber }
    where
        parsedFrom = matches !! 2
        parsedTo = matches !! 3
        parsedNumber = matches !! 1
        matches = map read (getIntsFromMoveLine input) :: [Int]

getIntsFromMoveLine :: String -> [String]
getIntsFromMoveLine input = getAllTextSubmatches (input =~ "move ([[:digit:]]+) from ([[:digit:]]+) to ([[:digit:]]+)") :: [String]

parseIdLine :: String -> [Int]
parseIdLine input = map read stringMatches :: [Int]
    where
        stringMatches = getAllTextMatches (input =~ "([0-9]+)") :: [String]
