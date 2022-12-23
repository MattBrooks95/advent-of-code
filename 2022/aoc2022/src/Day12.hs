module Day12 (
    run
    ) where

import Prelude hiding (
    getChar
    )

import Data.Char (
    ord
    )

import Data.Maybe

import System.Exit (
    die
    )

import Control.Monad (
    when
    )

import Data.List

run :: [String] -> IO ()
run inputLines = do
    let lengthOfRow = length $ head inputLines
    let asOneInput = concat inputLines
    print asOneInput
    let startIndex = elemIndex 'S' asOneInput
    when (isNothing startIndex) (die "no start element")
    let (path, _) = findPath lengthOfRow [] asOneInput [fromJust startIndex] (fromJust startIndex)
    case path of
        Nothing -> print "no path found?!?"
        Just p -> print $ "path:" ++ p ++ " path length:" ++ show (length p)

findPath :: Int -> [Char] -> [Char] -> [Bool] -> Int -> (Maybe [Char], [Bool])
findPath _ _ [] inspectedList _ = (Nothing, inspectedList)
findPath itemsPerRow currPath graph inspectedList idx = let currChar = graph !! idx in
    case find (== 'E') (catMaybes [upC, downC, leftC, rightC]) of
        Nothing -> do
            let directions = [up, down, left, right]
            --let inspectNextItems = filter (charIsHigher currChar . isJust . fst) directions
            let inspectNextItems = filter (\x -> isJust (fst x) && charIsHigher currChar (fromJust $ fst x)) directions
            let searchResults = map (\(nextChar, nextIdx) -> findPath itemsPerRow (currPath++[nextChar]) graph (inspectedList++concat (map snd directions)) nextIndex) inspectNextItems
            let foundPaths = filter (isJust . fst) searchResults
            let updatedInspectElementsList = inspectedList++concatMap (snd) searchResults
            if null foundPaths then (Nothing, inspectedList++updatedInspectElemenstList)
            else do
                let shortestPath = minimum (map fst foundPaths) in
                    (Just shortestPath, updatedInspectElementsList)
            --(Nothing, inspectedList ++ map (\x -> (x, False)) [upIdx, downIdx, leftIdx, rightIdx])
        Just finalChar -> (Just (currPath++[finalChar]), inspectedList)
        where
            currChar = graph !! idx
            up@(upC, upIdx) = getChar graph (idx - itemsPerRow)
            down@(downC, downIdx) = getChar graph (idx + itemsPerRow)
            left@(leftC, leftIdx) = getChar graph (idx - 1)
            right@(rightC, rightIdx) = getChar graph (idx + 1)

getChar :: [Char] -> Int -> (Maybe Char, Int)
getChar [] idx = (Nothing, idx)
getChar graph idx = if idx > length graph - 1 || idx < 0 then (Nothing, idx) else (Just $ graph !! idx, idx)

charIsHigher :: Char -> Char -> Bool
charIsHigher a b = (ord a) < (ord b)

