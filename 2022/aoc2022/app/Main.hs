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
    )

import Day4 (
    )

import Day5 (
    run
    )

import Day6 (
    run
    )

import Day7 (
    run
    )

import Day8 (
    run
    )

import Day9 (
    run
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
    --Day5.run fileLines
    --Day6.run fileLines
    --Day7.run fileLines
    Day8.run fileLines
    --Day9.run fileLines

