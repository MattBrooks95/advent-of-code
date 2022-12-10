module Day5 (
    run
    , parseMove
    , getIntsFromMoveLine
    , takeFromFrontOfString
    ) where

import Data.List (
    find
    , sortBy
    )

import Prelude hiding (id)

import qualified Data.Text as T (
    pack
    , splitOn
    , unpack
    , Text(..)
    )

import Data.Maybe (
    mapMaybe
    , fromJust
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
    }
instance Show Stack where
    show (Stack { stackContents=c, stackId=i} ) = show i ++ "|" ++ c

data Move = Move { from::Int, to::Int, number::Int }
instance Show Move where
    show (Move { from=f, to=t, number=n }) = show f ++ "->" ++ show t ++ "(" ++ show n ++ ")"

data CratesSimulation = CratesSimulation {
    stacks :: [Stack]
    , moves :: [Move]
    }
instance Show CratesSimulation where
    show (CratesSimulation {stacks=s, moves=m }) = (show $ sortBy (\x y -> compare (stackId x) (stackId y)) s) ++ "\n" ++ (show $ m)

runSimulation :: Bool -> CratesSimulation -> CratesSimulation
runSimulation needsReversed sim@(CratesSimulation _ []) = sim
runSimulation needsReversed sim = runSimulation needsReversed (applyMove needsReversed sim)

applyMove :: Bool -> CratesSimulation -> CratesSimulation
applyMove needsReversed sim@(CratesSimulation _ []) = sim
applyMove needsReversed sim@(CratesSimulation simStacks (mv:mvs)) =
    let (remainingCrates, takenCrates) = takeFromFrontOfString (stackContents fromStack) numCratesToMove in
    let newSim = sim {
        stacks =
            otherStacks
            ++ [(fromStack { stackContents = remainingCrates })]
            ++ [(toStack { stackContents = (if needsReversed then reverse takenCrates else takenCrates) ++ (stackContents toStack) })]
        , moves = mvs
    } in
    --trace (show (remainingCrates, takenCrates)) newSim
    trace (show newSim) newSim
    where
        -- I'm using fromJust because the stacks should always exist
        fromStack = fromJust $ find (\x -> stackId x == from mv) simStacks
        toStack = fromJust $ find (\x -> stackId x == to mv) simStacks
        otherStacks = filter (\x -> stackId x /= stackId fromStack && stackId x /= stackId toStack) simStacks
        numCratesToMove = number mv

takeFromFrontOfString :: String -> Int -> (String, String)
takeFromFrontOfString str num = (drop num str, take num str)

run :: [String] -> IO ()
run inputLines = do
    let cratesSimulation = parseDay5 inputLines
    print "day5"
    print "before:"
    print cratesSimulation
    --print (stacks cratesSimulation)
    -- part one moves crates one at a time, so their post-move ordering is the opposite
    -- of what it was before the move
    -- part two, all the crates are moved at once so they no longer need reversed
    let result = runSimulation True cratesSimulation
    let topCratesPerStack = map (\x -> (stackId x, getTopCrate x)) (stacks result)
    print "after:"
    print result
    print "top crates part one:"
    print $ sortBy (\(x,_) (y,_) -> compare x y) topCratesPerStack
    print "top crates part two:"
    let partTwoResult = runSimulation False cratesSimulation
    let partTwoTopCrates = map (\x -> (stackId x, getTopCrate x)) (stacks partTwoResult)
    print $ sortBy (\(x,_) (y,_) -> compare x y) partTwoTopCrates

getTopCrate :: Stack -> Maybe Char
getTopCrate (Stack contents _) = topCrate
    where
        topCrate = if length contents >= 1 then Just $ head contents else Nothing
        --topCrate = case contents of
        --    [_, x] -> Just x
        --    [x] -> Just x
        --    [] -> Nothing


parseDay5 :: [String] -> CratesSimulation
parseDay5 inputLines = CratesSimulation { stacks=parsedStacks, moves = parsedMoves }
    where
        (crateLines, stackIdLine, moveLines) = organizeLines inputLines
        parsedMoves = reverse $ parseMoves moveLines
        parsedIdLine = parseIdLine stackIdLine
        --parsedIdLine = trace (show $ parseIdLine stackIdLine) (parseIdLine stackIdLine)
        parsedCrates = concatMap parseCrates crateLines
        --parsedCrates = trace (show $ concatMap parseCrates crateLines) (concatMap parseCrates crateLines)
        -- parsedStacks is fine, problem is after this somewhere
        parsedStacks = map (`buildStackFromCrates` parsedCrates) parsedIdLine

buildStackFromCrates :: Int -> [(Maybe Char, Int)] -> Stack
buildStackFromCrates parsedId potentialCrates =
    Stack {
        stackId=parsedId
        , stackContents=justCrates
        }
    where
        justCrates = mapMaybe fst printCrates
        printCrates = trace (show cratesForStackId) cratesForStackId
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
parseCrate input = if length input >= 3 && secondElem /= ' ' then
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
