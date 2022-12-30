{-# LANGUAGE LambdaCase #-}
module Day12 (
    run
    ) where

import Prelude hiding (
    getChar
    )

import Control.Monad (
    when
    )

import System.Exit

import Data.Char (
    ord
    )

import GHC.Real

import Data.Foldable

import Data.Maybe

import Debug.Trace

import qualified Data.Set as S

import Lib (
    indexedList
    )

import qualified Data.Vector as V
import qualified Data.List as L

type LabeledChar = (Char, Int, Index)
type Index = Int
data Node = Node Char Int Index [Index]
instance Show Node where
    show (Node c val idx neighbors) = "(" ++ [c] ++ ":" ++ show idx ++ " " ++ show neighbors ++ ")"
nIndex (Node _ _ idx _) = idx

-- a node with it's distance to the endpoint and which path to take
--data PathNode = PathNode Node (Maybe (Int, Index)) deriving (Show)

data Color = White | Gray | Black deriving (Show)
data Parent = Parent Index | Null deriving (Show)
data PaintedNode = PaintedNode Node Color Int Parent
instance Show PaintedNode where
    show (PaintedNode node color dist parent) = "[" ++ show node ++ " " ++ show color ++ " dist:" ++ show dist ++ " parent:" ++ (\case { Parent x -> show x; Null -> show Null;}) parent ++ "]"

pNodeIndex :: PaintedNode -> Index
pNodeIndex (PaintedNode node _ _ _) = nIndex node

pNodeDist :: PaintedNode -> Int
pNodeDist (PaintedNode _ _ dist _) = dist

pUpdateDist :: PaintedNode -> Int -> PaintedNode
pUpdateDist (PaintedNode node color _ parent) newDist = PaintedNode node color newDist parent

--pNodeIsChecked (PathNode _ _) = True
--pNodeIsChecked _ = False

--pNodeIdx (PathNode (Node _ _ idx _) _) = Just idx
--pNodeIdx Unchecked = Nothing

--getPathInfo :: PathNode -> Maybe (Int, Index)
--getPathInfo Unchecked = Nothing
--getPathInfo (PathNode _ pathInfo) = pathInfo

-- after struggling for hours to do this recursively, I consulted chapter 20.2 in Introduction to Algorithms Fourth Edition
-- Cormen, Leiserson, Rivest and Stein

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
            --print nodesWithNeighbors
            --let graphWithDistances = makeGraphWithDistances nodesWithNeighbors (V.replicate (length nodesWithNeighbors) Unchecked) 'E' (trace ("starting with:" ++ show sIdx) sIdx) S.empty
            print $ "starting at index:" ++ show sIdx
            let useStartIdx = sIdx
            -- initialize the graph
            let startingGraph = V.map (\x -> PaintedNode x White (-1) Null) nodesWithNeighbors
            let startingNode = startingGraph V.!? sIdx
            when (isNothing startingNode) $ do
                print ("couldn't find node at index" ++ show sIdx ++ " to start processing from!")
                exitFailure
            print $ "starting from" ++ show useStartIdx
            let answer = shortestPath (V.update startingGraph (V.singleton (sIdx, whiteNodeToGray (fromJust startingNode) 0 Null))) [sIdx]
            mapM_ print answer
            let endItem = V.find (\(PaintedNode (Node c _ _ _) _ _ _) -> c == 'E') answer
            print endItem
            --mapM_ print (V.map (\x -> (pNodeIndex x, pNodeDist x)) answer)
            --let (_, answers) = shortestPath nodesWithNeighbors 'E' useStartIdx S.empty (V.replicate (length nodesWithNeighbors) Nothing)
            --case answers of
            --    Nothing -> print "no answer!!! = ("
            --    Just answer -> do
            --        print $ show (head answer)

shortestPath :: V.Vector PaintedNode -> [Index] -> V.Vector PaintedNode
shortestPath graph [] = graph
--shortestPath graph (currIdx:remainingQ) = case graph V.!? (trace ("=================" ++ show currIdx) currIdx) of
shortestPath graph (currIdx:remainingQ) = case graph V.!? currIdx of
    Nothing -> trace ("bad index, elem does not exist:" ++ show currIdx) graph
    Just (PaintedNode thisNode@(Node _ _ _ neighbors) _ dist thisParent) -> let neighborNodes = mapMaybe (graph V.!?) neighbors in
        let whiteNodes = filter (\case { (PaintedNode _ White _ _) -> True; _ -> False; }) neighborNodes in
        --let updatedNodes = map (\(PaintedNode node@(Node _ _ nIdx _) _ _ _) -> (nIdx, PaintedNode node Gray (dist + 1) (Parent currIdx))) whiteNodes in
        let distOfNeighbors = dist + 1 in
        --let updatedNodes = map (\whiteNode -> (pNodeIndex whiteNode, whiteNodeToGray whiteNode distOfNeighbors currIdx)) (trace ("dist:" ++ show dist ++ " neigh:" ++ show whiteNodes) whiteNodes) in
        let updatedNodes = map (\whiteNode -> (pNodeIndex whiteNode, whiteNodeToGray whiteNode distOfNeighbors (Parent currIdx))) whiteNodes in
        -- the initial element will need to have it's parent set to 0
        -- all other nodes will have their parent set when we change their color from white to gray
        let nodeParent = if dist == 0 then Null else thisParent in
        let enqueueIndices = map (pNodeIndex . snd) updatedNodes in
        let nextQueue = remainingQ++easyTrace enqueueIndices in
        shortestPath (graph V.// ((currIdx, PaintedNode thisNode Black dist nodeParent):updatedNodes)) (easyTrace nextQueue)

whiteNodeToGray :: PaintedNode -> Int -> Parent -> PaintedNode
whiteNodeToGray (PaintedNode node _ _ _) dist newParent = PaintedNode node Gray dist newParent

--            let graphWithDistances = makeGraphWithDistances nodesWithNeighbors (V.replicate (length nodesWithNeighbors) Unchecked) 'E' useStartIdx S.empty
--            print graphWithDistances
--            print $ "shortest path length:" ++ show (case getPathInfo (graphWithDistances V.! useStartIdx) of { Nothing -> "no path"; Just (dist, _) -> show dist; })
--            --let shortestPath = getShortestPath 'E' graphWithDistances sIdx
--            let shortestPath = getShortestPath 'E' graphWithDistances useStartIdx
--            case shortestPath of 
--                Nothing -> print "no shortest path!"
--                Just sPath -> do
--                    print sPath
--                    print $ "shortest path length" ++ show (length sPath - 1)


-- trying to memoize the results of the evaluation of the sub trees to get a runtime that
-- will actually finish
-- the non-memoized solution works but seemingly will not terminate on the full input
-- because it's constantly re-evaluated sub trees
-- the memoized version doesn't work either...
-- it kind of looks like it's looping
--type Memo = V.Vector (Maybe [(Int, [Index])])
--shortestPath :: V.Vector Node -> Char -> Index -> S.Set Index -> Memo -> (Memo, Maybe [(Int, [Index])])
--shortestPath graphIn endChar thisIdx alreadyVisited memo =
--    case memo V.!? trace ("thisIdx:" ++ show thisIdx) thisIdx of
--        Just memoAnswer@(Just _) -> (memo, trace ("memoed answer:" ++ show memoAnswer) memoAnswer)
--        _ ->
--            if S.member thisIdx (trace (show $ "num alreadyVisited" ++ (show . length) alreadyVisited) alreadyVisited) then (memo, Nothing)
--            else
--                let newAlreadyVisited = S.insert thisIdx alreadyVisited in
--                case graphIn V.!? thisIdx of
--                    Nothing -> trace ("bad index:" ++ show thisIdx) (memo, Nothing)
--                    Just thisNode@(Node c _ _ neighbors) ->
--                        if c == endChar then let newPath = Just [(0, [thisIdx])] in
--                            (V.update memo (V.singleton (thisIdx, newPath)), newPath)
--                        --else let (_, neighborSolutions) = foldr (\x (prevDone, prevMemo, answers) -> (S.insert x done, shortestPath graphIn endChar x (S.union newAlreadyVisited done) prevMemo:answers)) (S.empty, memo, []) neighbors in
--                        else let (_, finalMemo, neighborSolutions) = foldr (\x (prevDone, prevMemo, answers) -> processNeighbor (shortestPath graphIn endChar) answers x prevDone prevMemo) (newAlreadyVisited, memo, []) neighbors in
--                        --else let (_, finalMemo, neighborSolutions) = foldl' (\(prevDone, prevMemo, answers) x -> processNeighbor (shortestPath graphIn endChar) answers x prevDone prevMemo) (newAlreadyVisited, memo, []) neighbors in
--                            let justSolutions = filter (not . null) $ catMaybes neighborSolutions in
--                            --if null justSolutions then (V.update finalMemo (V.singleton (thisIdx, Nothing)), Nothing)
--                            if null justSolutions then (V.update finalMemo (V.singleton (thisIdx, Nothing)), Nothing)
--                            else let answerNeighbor = L.minimumBy (\listA listB -> (fst . head) listA `compare` (fst . head) listB) justSolutions in
--                                let myCount = (fst . head) answerNeighbor + 1 in
--                                let myPathList = thisIdx:(snd . head) answerNeighbor in
--                                (V.update finalMemo (V.singleton (thisIdx, Just [(myCount, myPathList)])), Just [(myCount, myPathList)])

--processNeighbor :: (Index -> S.Set Index -> Memo -> (Memo, Maybe [(Int, [Index])])) -> [Maybe [(Int, [Index])]] -> Index -> S.Set Index -> Memo -> (S.Set Index, Memo, [Maybe [(Int, [Index])]])
--processNeighbor runNeighborFunc otherSolutions nextIdx useAlreadyVisited useMemo =
--    let (newMemo, newAnswer) = runNeighborFunc nextIdx useAlreadyVisited useMemo in
--    let newAlreadyVisited = S.insert nextIdx useAlreadyVisited in
--    --let newMemo = V.update useMemo (V.singleton (nextIdx, newAnswer)) in
--    let newAnswerList = newAnswer:otherSolutions in
--    (newAlreadyVisited, newMemo, newAnswerList)

--getShortestPath :: Char -> V.Vector PathNode -> Int -> Maybe [Index]
--getShortestPath endChar graph sIdx =
--    case graph V.! sIdx of
--        Unchecked -> Nothing
--        PathNode (Node c _ _ _) pathInfo -> 
--            case pathInfo of
--                Nothing -> Nothing
--                Just (_, nextNode) ->
--                    if endChar == c then Just [sIdx]
--                    else 
--                        let nextNodes = getShortestPath endChar graph nextNode in
--                        case nextNodes of
--                            Nothing -> Nothing
--                            Just nextIndices -> Just $ sIdx:nextIndices

--makeGraphWithDistances :: V.Vector Node -> V.Vector PathNode -> Char -> Int -> S.Set Index -> V.Vector PathNode
--makeGraphWithDistances graphIn graphOut endChar startIndex alreadyVisited =
--    if S.member startIndex alreadyVisited then graphOut
--    else
--        let thisNode@(Node c _ idx neighbors) = graphIn V.! startIndex in
--        let thisPathNode = graphOut V.! startIndex in
--            --case trace ("visiting idx:" ++ show startIndex ++ " node:" ++ show thisPathNode ++ " neighbors:" ++ show neighborNodes ++ " visited list:" ++ show alreadyVisited) thisPathNode of
--            --case trace ("visiting idx:" ++ show startIndex ++ " node:" ++ show thisPathNode ++ " neighbors:" ++ show neighborNodes) thisPathNode of
--            case thisPathNode of
--                PathNode _ _ -> graphOut
--                Unchecked ->
--                    if c == endChar then V.update graphOut $ V.singleton (idx, PathNode thisNode (Just (0, idx)))
--                    else
--                        --let graphWithMeAsNode = V.update graphOut (V.singleton (idx, PathNode thisNode Nothing)) in
--                        let newVisitedList = S.insert startIndex alreadyVisited in
--                        let (updatedSubGraph, _) = foldr (\x (newGraph, doneNeighbors) -> (makeGraphWithDistances graphIn newGraph endChar x (easyTrace $ S.union newVisitedList doneNeighbors), S.insert x doneNeighbors)) (graphOut, S.empty) neighbors in
--                        let neighborElems = trace ("elem" ++ show startIndex ++ " " ++ show (map (updatedSubGraph V.!) neighbors)) (map (updatedSubGraph V.!) neighbors) in
--                        let shortestPathNeighbor = getShortestPathNeighbor neighborElems in
--                            case shortestPathNeighbor of
--                                Nothing -> V.update updatedSubGraph (V.singleton (startIndex, PathNode thisNode Nothing))
--                                Just shortest ->
--                                    let newLength = (fst . fromJust . getPathInfo) shortest + 1 in
--                                    let newElem = V.singleton (idx, PathNode thisNode (Just (newLength, (fromJust . pNodeIdx) shortest))) in
--                                    V.update updatedSubGraph newElem

easyTrace :: Show a => a -> a
easyTrace printMe = trace (show printMe) printMe

--getShortestPathNeighbor :: [PathNode] -> Maybe PathNode
--getShortestPathNeighbor neighbors =
--    let checkedNeighbors = filter (\x -> pNodeIsChecked x && isJust (getPathInfo x)) neighbors in
--    if null checkedNeighbors then Nothing
--    else 
--        let sortedLengths = L.sortBy compareNodeLengths checkedNeighbors in
--        let shortestPathNeighbor = Just $ head sortedLengths in
--            shortestPathNeighbor
--        --let shortestPathNeighbor = Just $ head $ trace (" sorted lengths:" ++ show sortedLengths) sortedLengths in
--        --    trace ("neighbors:" ++ show neighborNodes ++ "lengths:" ++ show sortedLengths ++ " shortestPath neighbor:" ++ show shortestPathNeighbor) shortestPathNeighbor
--            --shortestPathNeighbor

--compareNodeLengths :: PathNode -> PathNode -> Ordering
--compareNodeLengths Unchecked _ = GT
--compareNodeLengths _ Unchecked = LT
--compareNodeLengths pNodeA pNodeB = 
--    case pNodeALength of
--        Just (aLen, _) -> case pNodeBLength of
--            Nothing -> LT
--            Just (bLen, _) -> aLen `compare` bLen
--        Nothing -> GT
--    where
--        pNodeALength = getPathInfo pNodeA
--        pNodeBLength = getPathInfo pNodeB


-- I had to re-read the problem:
--    "Your current position (S) has elevation a, and the location that
--    should get the best signal (E) has elevation z."
evaluateCharacter :: Char -> Int
evaluateCharacter c = case c of
    'E' -> ord 'z'
    'S' -> ord 'a'
    normLetter -> ord normLetter

-- Left -> didn't find a path, returns inspected nodes
-- Right -> found the path, returns the indices of the path
-- TODO visited list isn't really a visited list, it needs to be a "this is a dead-end" list
--findEnd :: V.Vector Node -> Char -> S.Set Index -> Index -> Either (S.Set Index) (S.Set Index, [Index])
--findEnd graph endChar visitedList startIdx = case thisNode of
--    Nothing -> trace ("bad node idx:" ++ show startIdx) Left (S.insert startIdx visitedList)
--    Just (Node c _ idx neighbors) ->
--        let canCheckNeighbors = filter (not . flip S.member visitedList) neighbors in
--        if trace ("checking:" ++ [c] ++ " " ++ show idx ++ " w/checkable neighbors" ++ show canCheckNeighbors ++ " already checked list:" ++ show visitedList) startIdx `elem` visitedList then Left visitedList
--        else
--            if c == endChar
--            then Right (visitedList, [idx])
--            else 
--                --let possibleSolutions = foldr (\x (deadEndIndices, foundPaths) -> case findEnd graph endChar (S.union deadEndIndices (S.union visitedList (S.fromList $ L.delete x canCheckNeighbors))) x of {
--                let possibleSolutions = foldr (\x (deadEndIndices, foundPaths) -> case findEnd graph endChar (S.union deadEndIndices visitedList) x of {
--                    Left newDeadEnds -> (S.union visitedList newDeadEnds, foundPaths);
--                    Right (oldDeadEnds, pathToAnswer) -> (S.insert x oldDeadEnds, foundPaths++[x:pathToAnswer]);
--                    }) (S.insert startIdx visitedList, []) canCheckNeighbors in
--                if not $ didSolutionExist possibleSolutions then Left (S.union visitedList (fst possibleSolutions))
--                else Right (S.insert startIdx (fst possibleSolutions), getShortestList (snd possibleSolutions))

--    where
--        thisNode = getNodeFromGraph graph startIdx
--        numVisitedNodes = show $ length visitedList

--didSolutionExist :: (S.Set Index, [[Index]]) -> Bool
----didSolutionExist possibleSolutions = trace traceMsg (numPossibleSolutions /= 0)
--didSolutionExist possibleSolutions = numPossibleSolutions /= 0
--    where
--        traceMsg = show $ "num possible solutions:" ++ show numPossibleSolutions
--        numPossibleSolutions = (sum . map length . snd) possibleSolutions

--getShortestList :: Show a => [[a]] -> [a]
--getShortestList lists = trace ("sol length" ++ show (length (head lists)) ++ " solution lists:"++show sortedLists ++ "num lists" ++ show (length sortedLists)) (head sortedLists)
--    where
--        sortedLists = L.sortOn length lists

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
