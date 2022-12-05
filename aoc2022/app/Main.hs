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

import Data.Maybe (
    fromMaybe
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
    , Rucksack(..), findSameItems
    , prioritiesMap
    )

import Lib (
    groupLines
    )

import Data.Maybe (
    fromJust
    , mapMaybe
    , isJust
    , catMaybes
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
    dayThree fileLines

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
    mapM_ print rucksacksWithDupes
    --let rucksacksWithDupes = map (\r@(Rucksack left right) -> (r, findDuplicates left right)) rucksacks
    --mapM_ print rucksacksWithDupes
    let dupes = map (head . snd) rucksacksWithDupes
    let dupePriorities = foldr (\item acc -> acc + fromMaybe 0 (M.lookup item prioritiesMap)) 0 dupes :: Int
    print dupePriorities
