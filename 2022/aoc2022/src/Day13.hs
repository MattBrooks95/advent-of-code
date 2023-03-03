module Day13 (
    run
    ) where

import Lib (
    groupLines
    )

import Text.Regex.TDFA
import qualified Data.Text as T

run :: [String] -> IO ()
run inputLines = do
    --print inputLines
    let linesGrouped = groupLines inputLines
    mapM_ print linesGrouped


data Input a = EmptyList | List [Input a] | Atom [a]

parseInputPairs :: [[String]] -> [(Input a, Input a)]
parseInputPairs groupedLines = []

parseInput :: Read a => String -> Input a
parseInput input = if input == "[]" then EmptyList
    else
        if input =~ "(.*,)+" :: Bool then do
            let splitOnComma = T.splitOn (T.pack ",") (T.pack input)
            List $ map (parseInput . T.unpack) splitOnComma
        else Atom (read input)
