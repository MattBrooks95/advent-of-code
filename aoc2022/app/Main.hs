module Main (main) where

import Prelude hiding (
    lines
    )
import qualified Prelude as P (
    lines
    )

import System.Environment
import System.Exit (
    exitFailure
    )
import System.Directory (
    makeAbsolute
    )
import System.IO (
    readFile'
    )

import qualified Data.Map as M (
    lookup
    )

import Data.Either (
    rights
    )

import Day1 (
    parseElf, findElfWithHighestCalories, sortElvesByHighestCalories, elvesWithTotalCalories
    )

import Day2 (
    read
    , score
    , Game(..)
    , reinterpretGame
    )

import Day3 (
    parseRucksack
    , Rucksack(..)
    , findSameItems
    , prioritiesMap
    , getMatchingItemsForRucksacks
    , prioritiesMap
    )

import Day4 (
    lineToRangePair
    , aContainsB
    , bContainsA
    , rangeOverlaps
    )

import Day5 (
    run
    )

import Lib (
    groupLines
    , chunk
    )

import Data.Maybe (
    fromJust
    , mapMaybe
    , isJust
    , catMaybes
    , fromMaybe
    )

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            print "must provide path to input file"
            exitFailure
        (x:_) -> print $ "using path:" ++ x
    filePath <- makeAbsolute $ head args
    print filePath
    fileContents <- readFile' filePath
    let fileLines = P.lines fileContents
    --print fileLines
    --dayOne fileLines
    --dayTwo fileLines
    --print "using abs path:" ++ filePath ++ "for input"
    --dayThree fileLines
    --dayFour fileLines
    Day5.run fileLines

dayOne :: [String] -> IO()
dayOne [] = print "error, no file contents"
dayOne lines = do
    let groupedLines = groupLines lines
    --print "grouped lines:"
    --print groupedLines
    let parsedElves = map parseElf groupedLines
    let elves = map fromJust (filter (\x -> case x of { Just e -> True; _ -> False }) parsedElves)
    -- part1
    print $ findElfWithHighestCalories elves
    -- part2
    let elvesCaloriesDescending = sortElvesByHighestCalories $ elvesWithTotalCalories elves
    if length elvesCaloriesDescending > 3
        then do
            let three = take 3 elvesCaloriesDescending
            print "top 3 elves:"
            print three
            let sumOfTopThree = sum $ map snd(take 3 elvesCaloriesDescending)
            print sumOfTopThree 
        else do
            print "can't find top 3 elves with the most calories, not enough elves"
            exitFailure

dayTwo :: [String] -> IO ()
dayTwo inputLines = do
    --print "TODO day two"
    --print inputLines
    let potentialGames = map Day2.read inputLines
    --print potentialGames
    let games = catMaybes potentialGames
    let gameScores = map score games
    --print gameScores
    print $ sum gameScores
    -- below is part 2
    let riggedGames = map reinterpretGame games
    --print $ take 5 games
    --print $ take 5 riggedGames
    --I got my star but I'm not proud of how I handled this
    --I wish I could abstrackt away some of the cases statements used to
    --determine which choice beats, loses or draws to which choice
    print $ sum (map score riggedGames)


dayThree :: [String] -> IO ()
dayThree inputLines = do
    let rucksacks = map parseRucksack inputLines
    print rucksacks
    let rucksacksWithDupes = map findSameItems rucksacks
    --mapM_ print rucksacksWithDupes
    --let rucksacksWithDupes = map (\r@(Rucksack left right) -> (r, findDuplicates left right)) rucksacks
    --mapM_ print rucksacksWithDupes
    let dupes = map (head . snd) rucksacksWithDupes
    let dupePriorities = foldr (\item acc -> acc + fromMaybe 0 (M.lookup item prioritiesMap)) 0 dupes :: Int
    print dupePriorities
    let rucksackGroups = chunk 3 rucksacksWithDupes
    case rucksackGroups of
        Left err -> print err
        Right groups -> do
            let badgesForGroup = map (\x -> getMatchingItemsForRucksacks (map fst x)) groups
            let prioritiesForBadges = map (\x -> M.lookup (head x) prioritiesMap) (rights badgesForGroup)
            let prioritiesSum = sum (catMaybes prioritiesForBadges)
            print prioritiesSum
            --mapM_ (putStrLn . \(x:_) -> "badgeLetter: " ++ x ++ " priority:" ++ (rights badgesForGroup)

dayFour :: [String] -> IO ()
dayFour lines = do
    let rangePairs = mapMaybe lineToRangePair lines
    print "PART ONE==================="
    print $ take 5 rangePairs
    print $ "total num of pairs:" ++ showLength rangePairs
    let rangesWithSuperset = filter (\x -> aContainsB x || bContainsA x) rangePairs
    print $ take 5 rangesWithSuperset
    print $ "num of pairs with superset:" ++ showLength rangesWithSuperset
    --print overlappingRanges
    print "PART TWO ================="
    let overlappingRanges = filter rangeOverlaps rangePairs
    print $ take 5 overlappingRanges
    print $ "num of pairs that overlap at all:" ++ showLength overlappingRanges
    where
        showLength = show . length
