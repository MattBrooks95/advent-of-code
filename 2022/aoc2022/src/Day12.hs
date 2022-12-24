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

import Debug.Trace

import Lib (
    indexedList
    )

import qualified Data.Vector as V
import qualified Data.List as L

type LabeledChar = (Char, Int, Index)
type Index = Int
data Node = Node Char Int Index [Index] deriving (Show)

run :: [String] -> IO ()
run inputLines = do
    let lengthOfRow = length $ head inputLines
    let asOneInput = concat inputLines
    --print asOneInput
    let startIndex = L.elemIndex 'S' asOneInput
    case startIndex of
        Nothing -> print "couldn't find start index!!!"
        Just sIdx -> do
            let charsWithIndices = indexedList asOneInput
            --print charsWithIndices
            let charsWithIndicesAndValues = map (\(x,idx) -> (x, evaluateCharacter x, idx)) charsWithIndices
            --print charsWithIndicesAndValues
            let asVector = V.fromList charsWithIndicesAndValues
            --print asVector
            let nodesWithNeighbors = makeGraph lengthOfRow asVector
            -- TODO some nodes are having their height values swapped with
            -- the index somewhere...
            print nodesWithNeighbors
            let path = findEnd nodesWithNeighbors 'E' [] sIdx
            case path of
                Just p -> print $ "found path:" ++ show p ++ " of length:" ++ show (length p - 1)
                Nothing -> print "couldn't find a path!!!!"
            --print path

-- I had to re-read the problem:
--    "Your current position (S) has elevation a, and the location that
--    should get the best signal (E) has elevation z."
evaluateCharacter :: Char -> Int
evaluateCharacter c = case c of
    'E' -> ord 'z'
    'S' -> ord 'a'
    normLetter -> ord normLetter

findEnd :: V.Vector Node -> Char -> [Index] -> Index -> Maybe [Index]
findEnd graph endChar visitedList startIdx = case thisNode of
    Nothing -> trace ("bad node idx:" ++ show startIdx) Nothing
    Just (Node c _ idx neighbors) ->
        if startIdx `elem` visitedList then Nothing
        else
            if c == endChar
            then Just [idx]
            else let possibleSolutions = map (findEnd graph endChar (idx:visitedList)) neighbors in
                let solutions = L.sortOn length (catMaybes possibleSolutions) in
                    if null solutions then Nothing else Just $ idx:head solutions
                --Just $ idx:head 
    where
        thisNode = getNodeFromGraph graph startIdx

makeGraph :: Int -> V.Vector LabeledChar -> V.Vector Node
makeGraph numItemsPerRow labeledNodes = findNeighbors numItemsPerRow 0 labeledNodes V.empty

findNeighbors :: Int -> Int -> V.Vector LabeledChar -> V.Vector Node -> V.Vector Node
findNeighbors itemsPerRow currIdx graphIn graphOut =
    if currIdx >= V.length graphIn || currIdx < 0
    then graphOut
    else let newItem = Node currItemChar currItemVal currItemIdx canClimbToIndices in
        findNeighbors itemsPerRow (currIdx + 1) graphIn (V.snoc graphOut newItem)
        where
            (currItemChar, currItemVal, currItemIdx) = graphIn V.! currIdx
            canClimbToIndices = map (\(_, _, idx) -> idx) canClimbToItems
            canClimbToItems = filter (\(toChar, toVal, _) -> canClimbTo (currItemChar, currItemVal) (toChar, toVal)) justDirections
            justDirections = catMaybes [up, down, left, right]
            up = getNodeFromGraphIn (currIdx - itemsPerRow)
            down = getNodeFromGraphIn (currIdx + itemsPerRow)
            left = getNodeFromGraphIn (currIdx - 1)
            right = getNodeFromGraphIn (currIdx + 1)
            getNodeFromGraphIn = getNodeFromGraph graphIn

getNodeFromGraph :: V.Vector a -> Int -> Maybe a
getNodeFromGraph graph idx = if checkIndex (V.length graph) idx
    then Just $ graph V.! idx
    else Nothing

-- returns true if it is possible to climb from location a to location b
-- the special cases are 'S', which you can never climb to (you start there), and can always climb from
-- and 'E', the end, which you can always climb to
-- you can climb up at most one elevation
-- you can always climb down
-- I had to re-read the problem:
--    "Your current position (S) has elevation a, and the location that
--    should get the best signal (E) has elevation z."
canClimbTo :: (Char, Int) -> (Char, Int) -> Bool
--canClimbTo _ ('E', _) = True
--canClimbTo ('S', _) _ = True
--canClimbTo _ ('S', _) = False
--canClimbTo (_, fVal) (_, tVal) = trace ("climb from height:" ++ show fVal ++ " to height:" ++ show tVal) (tVal <= (fVal + 1))
canClimbTo (_, fVal) (_, tVal) = tVal <= fVal + 1

checkIndex :: Int -> Int -> Bool
checkIndex numItems targetIndex = targetIndex >= 0 && targetIndex < numItems

--findPath :: Int -> [Char] -> [Char] -> [Bool] -> Int -> (Maybe [Char], [Bool])
--findPath _ _ [] inspectedList _ = (Nothing, inspectedList)
--findPath itemsPerRow currPath graph inspectedList idx = let currChar = graph !! idx in
--    case find (== 'E') (catMaybes [upC, downC, leftC, rightC]) of
--        Nothing -> do
--            let directions = [up, down, left, right]
--            --let inspectNextItems = filter (charIsHigher currChar . isJust . fst) directions
--            let inspectNextItems = filter (\x -> isJust (fst x) && charIsHigher currChar (fromJust $ fst x)) directions
--            let searchResults = map (\(nextChar, nextIdx) -> findPath itemsPerRow (currPath++[nextChar]) graph (inspectedList++concat (map snd directions)) nextIndex) inspectNextItems
--            let foundPaths = filter (isJust . fst) searchResults
--            let updatedInspectElementsList = inspectedList++concatMap (snd) searchResults
--            if null foundPaths then (Nothing, inspectedList++updatedInspectElemenstList)
--            else do
--                let shortestPath = minimum (map fst foundPaths) in
--                    (Just shortestPath, updatedInspectElementsList)
--            --(Nothing, inspectedList ++ map (\x -> (x, False)) [upIdx, downIdx, leftIdx, rightIdx])
--        Just finalChar -> (Just (currPath++[finalChar]), inspectedList)
--        where
--            currChar = graph !! idx
--            up@(upC, upIdx) = getChar graph (idx - itemsPerRow)
--            down@(downC, downIdx) = getChar graph (idx + itemsPerRow)
--            left@(leftC, leftIdx) = getChar graph (idx - 1)
--            right@(rightC, rightIdx) = getChar graph (idx + 1)
--
--getChar :: [Char] -> Int -> (Maybe Char, Int)
--getChar [] idx = (Nothing, idx)
--getChar graph idx = if idx > length graph - 1 || idx < 0 then (Nothing, idx) else (Just $ graph !! idx, idx)
--
--charIsHigher :: Char -> Char -> Bool
--charIsHigher a b = (ord a) < (ord b)
--
