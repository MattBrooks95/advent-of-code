module Day8 (
    run
    ) where

import Prelude hiding (
    max
    )

import Lib (
    chunk
    )

import Data.Char (
    digitToInt
    )

import qualified Data.Ord as O (
    Down(..)
    , max
    )

import Data.List (
    sort
    , sortBy
    , sortOn
    )

run :: [String] -> IO ()
run inputLines = do
    print "day8"
    let forest = createGrid inputLines
    putStrLn "forest:"
    mapM_ print forest
    --let chunkedForest = chunk ((round . sqrt) ((fromIntegral . length) forest)) forest
    --_ <- case chunkedForest of
    --    Left str -> print $ "couldn't pretty print the trees, the chunk function couldn't make the sublists: " ++ str
    --    Right cf -> mapM_ (print . reverse) (reverse cf)
    --let visibleTrees = getVisibleTrees forest
    --print $ "# visible trees:" ++ show (length visibleTrees)
    putStrLn "trees marked visible:"
    let forestMarkedVisibleTrees = getVisibleTrees forest
    mapM_ print forestMarkedVisibleTrees
    putStrLn ("# of visible trees:" ++ show (length (filter isVisible (concat forestMarkedVisibleTrees))))
    putStrLn "part2============================="
    --let visibleFromTestTree = (getPossibleTreesVisableFromTree forest ((forest !! 2) !! 2))
    --print $ show visibleFromTestTree
    --print $ show (getTreesVisibleFromTree visibleFromTestTree)
    let treesWithNeighbors = map (getPossibleTreesVisableFromTree forest) (concat forest)
    --print $ "with neighbors" ++ show treesWithNeighbors
    let treesWithVisibleNeighbors = map getTreesVisibleFromTree treesWithNeighbors
    --print $ "with visible trees:"
    mapM_ print treesWithVisibleNeighbors
    let withScenicTotals = map getScenicScore treesWithVisibleNeighbors
    --print $ "scenic totals:" ++ show withScenicTotals
    print $ "best scenic total:" ++ show (maximum (map snd withScenicTotals))

data Tree = Tree {
    tHeight :: Int
    , tRow :: Int
    , tCol :: Int
    , isVisible :: Bool
    }

data Direction = E | N | W | S

directions :: [Direction]
directions = [E, N, W, S]

numDirections :: Int
numDirections = length directions

instance Show Tree where
    show (Tree h r c v) = "(h:" ++ show h ++ " r:" ++ show r ++ " c:" ++ show c ++ " v:" ++ show v ++ ")"

mkTree :: Int -> Int -> Int -> Bool -> Tree
mkTree th r c isVis = Tree { tHeight = th, tRow = r, tCol = c , isVisible = isVis }

-- Dim 1 is a row, left to right from the left edge
-- Dim 2 is a column, up to down from the top edge
createGrid :: [String] -> [[Tree]]
createGrid [] = []
createGrid inputs = createGrid' inputs 1 []

createGrid' :: [String] -> Int -> [[Tree]] -> [[Tree]]
createGrid' [] _ acc = acc
-- https://stackoverflow.com/questions/49144465/index-of-current-element-in-haskell-list-comprehension
createGrid' (x:xs) row acc = createGrid' xs (row + 1) (acc ++ [newTrees])
    where
        newTrees = [ mkTree h row idx False | (idx, h) <- zip [1..] treeHeights]
        treeHeights = map digitToInt x

-- for row of trees, mark all the trees that are taller
-- than all of the previous trees in that row as visible (able to be seen from the edge)
-- left to right, top to bottom (East -> West)
-- reverse the rows, then do the marking again (West -> East)
-- turn the cols into rows, do the marking again (North -> South)
-- reverse the cols, do the marking again (South -> North)
-- at the end of each operation, undo the row/col translation, so that the trees
-- can be visually compared to the solution when they are printed to the console
-- (they are put back into their original order)
getVisibleTrees :: [[Tree]] -> [[Tree]]
getVisibleTrees [] = [] 
getVisibleTrees allTrees = do
    let eastDone = map markTrees allTrees
    let westDone = map (markTrees . reverse) eastDone
    let northDone = colsAsRows True $ map markTrees (rowsAsCols False westDone)
    let southDone = colsAsRows False $ map markTrees (rowsAsCols True northDone)
    southDone
    where
        markTrees = markTreesVisible (-1)

rowsAsCols :: Bool -> [[Tree]] -> [[Tree]]
rowsAsCols flipColElemOrder trees =
    let filterResult = [ filter (\x -> tCol x == colIdx) (concat trees) | colIdx <- [1..length (head trees)]] in
    if flipColElemOrder then map reverse filterResult else filterResult

colsAsRows :: Bool -> [[Tree]] -> [[Tree]]
colsAsRows flipRowElemOrder trees =  
    let filterResult = [ filter (\x -> tRow x == rowIdx) (concat trees) | rowIdx <- [1..length trees]] in
    if flipRowElemOrder then map reverse filterResult else filterResult

markTreesVisible :: Int -> [Tree] -> [Tree]
markTreesVisible _ [] = []
markTreesVisible max (x@(Tree h r c _):xs) =
    if h <= max
    then x:markTreesVisible max xs
    else Tree h r c True:markTreesVisible h xs


-- (NorthTrees, STrees, ETrees, WTrees)
getPossibleTreesVisableFromTree :: [[Tree]] -> Tree -> (Tree, [Tree], [Tree], [Tree], [Tree])
getPossibleTreesVisableFromTree forest base@(Tree _ tr tc _) = (base, northTrees, southTrees, eastTrees, westTrees)
    where
        northTrees = filter (\(Tree _ row col _) -> tc == col && row < tr) flatForest
        southTrees = filter (\(Tree _ row col _) -> tc == col && row > tr) flatForest
        eastTrees = filter (\(Tree _ row col _) -> tr == row && tc < col) flatForest
        westTrees = filter(\(Tree _ row col _) -> tr == row && tc > col) flatForest
        flatForest = concat forest

getTreesVisibleFromTree :: (Tree, [Tree], [Tree], [Tree], [Tree]) -> (Tree, [Tree], [Tree], [Tree], [Tree])
getTreesVisibleFromTree (base@(Tree bh br bc _), nts, sts, ets, wts) = (base, vn, vs, ve, vw)
    where
        --vn = takeTillTaller (sortBy (\(Tree _ r1 _ _) (Tree _ r2 _ _) -> r1 > r2) nts) -- filter (\(Tree h r c _) -> True) []
        -- it says that the sortOn (Data.Ord.Down x) pattern is better
        --vn = takeTillTaller (reverse $ sortOn tCol nts) -- filter (\(Tree h r c _) -> True) []
        vn = takeTillTaller (sortOn (O.Down . tRow) nts) -- filter (\(Tree h r c _) -> True) []
        vs = takeTillTaller (sortOn tRow sts)
        ve = takeTillTaller (sortOn tCol ets)
        vw = takeTillTaller (sortOn (O.Down . tCol) wts)
        takeTillTaller = takeTreeTillTaller base

takeTreeTillTaller :: Tree -> [Tree] -> [Tree]
takeTreeTillTaller _ [] = []
takeTreeTillTaller base@(Tree bh br bc _) (add@(Tree h r c _):ts) =
    if bh <= h
    then [add]
    else add:takeTreeTillTaller base ts

getScenicScore :: (Tree, [Tree], [Tree], [Tree], [Tree]) -> (Tree, Int)
-- I thought that the 0's needed to be turned into ones, so that trees on the edge don't get screwed (one zero will ruin their score)
-- but from looking at their sample for part 2, I guess we're supposed to let it go to 0
--getScenicScore (baseTree, nts, sts, ets, wts) = (baseTree, foldr (*) 1 (map (max 1 . length) [nts, sts, ets, wts]))
getScenicScore (baseTree, nts, sts, ets, wts) = (baseTree, foldr (*) 1 (map length [nts, sts, ets, wts]))
