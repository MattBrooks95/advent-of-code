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
        go groupsAcc [] [] = groupsAcc
        go groupsAcc groupAcc [] = groupAcc:groupsAcc
        go groupsAcc groupAcc (x:xs) = case x of
            "" -> go (reverse groupAcc:groupsAcc) [] xs
            line -> go groupsAcc (line:groupAcc) xs

-- this will reverse the order of the groups, and the order of their contents
chunk :: Int -> [a] -> Either String [[a]] 
chunk sizeOfGroups origList
    | sizeOfGroups <= 0 || not dividesEvenly = Left errMsg
    | otherwise = Right $ chunk' sizeOfGroups origList 0 []
    where
        lenList = length origList
        errMsg = "list of length: " ++ show lenList ++ " cannot be evenly split into chunks of size:" ++ show sizeOfGroups
        dividesEvenly = lenList `mod` sizeOfGroups == 0

-- sizeOfGroups inputList counter accumulator
chunk' :: Int -> [a] -> Int -> [[a]] -> [[a]]
chunk' _ [] _ acc = acc
chunk' sizeOfGroups (x:xs) count [] = chunk' sizeOfGroups xs (count + 1) [[x]]
chunk' sizeOfGroups (x:xs) count (y:ys) = if startNewGroup
    then chunk' sizeOfGroups xs 0 ([x]:y:ys) 
    else chunk' sizeOfGroups xs (count + 1) ((x:y):ys)
    where
        startNewGroup = length (x:xs) `mod` sizeOfGroups == 0
