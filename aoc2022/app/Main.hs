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

import Day1 (
    parseElf, findElfWithHighestCalories, sortElvesByHighestCalories, elvesWithTotalCalories
    )

import Day2 (
    read
    , score
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
    dayTwo fileLines
    --print "using abs path:" ++ filePath ++ "for input"

dayOne :: [String] -> IO()
dayOne [] = print "error, no file contents"
dayOne lines = do
    let groupedLines = groupLines lines
    print "grouped lines:"
    print groupedLines
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
    print potentialGames
    let games = catMaybes potentialGames
    let gameScores = map score games
    print gameScores
    print $ sum gameScores


