module Lib where

import Prelude hiding (
    lines
    )

import qualified Data.Text as T

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
        -- it took weeks for this to pop up, but there was a bug here
        -- where the last group would remain reversed (because I failed to use reverse to put it back in order)
        --go groupsAcc groupAcc [] = reverse groupAcc:groupsAcc
        go groupsAcc groupAcc [] = reverse groupAcc:groupsAcc
        go groupsAcc groupAcc (x:xs) = case dropLeadingWhitespace x of
            "" -> go (reverse groupAcc:groupsAcc) [] xs
            line -> go groupsAcc (line:groupAcc) xs

dropLeadingWhitespace :: String -> String
dropLeadingWhitespace [] = ""
dropLeadingWhitespace (x:xs) =
    if x == ' '
    then dropLeadingWhitespace xs
    else x:xs

dropLeadingWhitespaceTxt :: T.Text -> T.Text
dropLeadingWhitespaceTxt txt = T.pack $ dropLeadingWhitespace (T.unpack txt)

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

-- find an item from the list, and remove it
-- returning the list of length N - 1 and the found item
getItemFromList :: (a -> Bool) -> [a] -> (Maybe a, [a])
getItemFromList _ [] = (Nothing, [])
getItemFromList p list = go [] list
    where
        go prevCheckedItems [] = (Nothing, prevCheckedItems)
        go prevCheckedItems (x:xs) =
            if p x
            then (Just x, prevCheckedItems++xs)
            else go (prevCheckedItems++[x]) xs

indexedList :: [a] -> [(a, Int)]
indexedList [] = []
indexedList origList@(_:_) = go origList 0 []
    where
        go :: [a] -> Int -> [(a, Int)] -> [(a, Int)]
        go [] _ acc = reverse acc 
        go (x:xs) currIdx acc = go xs (currIdx + 1) ((x, currIdx):acc)
