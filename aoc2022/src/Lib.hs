module Lib where

import Prelude hiding (
    lines
    )

-- when data is demarcated by newlines, and related data is demarcated
-- by empty lines, this function will throw away the empty lines and
-- return the lines of related data as lists
groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines lines = reverse $ go [] [] lines
    where
        -- groups currentGroup inputs result
        go :: [[String]] -> [String] -> [String] -> [[String]]
        go groupsAcc _ [] = groupsAcc
        go groupsAcc groupAcc (x:xs) = case x of
            "" -> go (groupAcc:groupsAcc) [] xs
            line -> go groupsAcc (line:groupAcc) xs
--groupLines lines = 

-- returns one group of consecutive non-empty lines up to an empty line
-- and then returns the rest of the input without the empty line
