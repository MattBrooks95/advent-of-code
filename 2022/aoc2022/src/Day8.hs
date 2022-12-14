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

import Data.List (
    sort
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

getVisibleTrees :: [[Tree]] -> [[Tree]]
getVisibleTrees trees = getVisibleTrees' trees

-- requires the list of trees to already by sorted by height
getVisibleTrees' :: [[Tree]] -> [[Tree]]
getVisibleTrees' [] = [] 
getVisibleTrees' allTrees = do
    let eastDone = filterEast allTrees
    let westDone = map reverse $ filterEast (map reverse eastDone)
    let northDone = rowsAsCols False $ filterEast (rowsAsCols False westDone)
    let southDone = rowsAsCols True $ filterEast (rowsAsCols True northDone)
    southDone

filterEast :: [[Tree]] -> [[Tree]]
filterEast trees = map (markTreesVisible (-1)) trees

rowsAsCols :: Bool -> [[Tree]] -> [[Tree]]
rowsAsCols flipColElemOrder trees =
    let filterResult = [ reverse $ filter (\x -> tCol x == colIdx) (concat trees) | colIdx <- [1..length (head trees)]] in
    if flipColElemOrder then map reverse filterResult else filterResult

colsAsRows :: Bool -> [[Tree]] -> [[Tree]]
colsAsRows flipRowElemOrder trees =  [ filter (\x -> (tRow x) == rowIdx) (concat trees) | rowIdx <- [1..length trees]]

markTreesVisible :: Int -> [Tree] -> [Tree]
markTreesVisible _ [] = []
markTreesVisible max (x@(Tree h r c _):xs) =
    if h <= max
    then x:markTreesVisible max xs
    else Tree h r c True:markTreesVisible h xs


