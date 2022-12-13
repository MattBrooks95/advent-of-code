module Day8 (
    run
    ) where

import Data.Char (
    digitToInt
    )

run :: [String] -> IO ()
run inputLines = do
    print "day8"
    let forest = createGrid inputLines []
    putStrLn $ "# rows:" ++ show (length forest)

data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast
dirList :: [Direction]
dirList = [
    East
    , NorthEast
    , North
    , NorthWest
    , West
    , SouthWest
    , South
    , SouthEast
    ]

mkVisDirections :: [(Direction, Bool)]
mkVisDirections = [ (direc, False) | direc <- dirList]

data Tree = Tree { height::Int, visDirections :: [(Direction, Bool)] }
mkTree :: Int -> Tree
mkTree th = Tree { height = th, visDirections = mkVisDirections }

-- Dim 1 is a row, left to right from the left edge
-- Dim 2 is a column, up to down from the top edge
createGrid :: [String] -> [[Tree]] -> [[Tree]]
createGrid [] acc = acc
createGrid (x:xs) acc = createGrid xs (acc ++ [map mkTree lineAsNums])
    where
        lineAsNums = map digitToInt x
