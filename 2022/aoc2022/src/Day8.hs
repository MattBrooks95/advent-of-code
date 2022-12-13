module Day8 (
    run
    ) where

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
    let chunkedForest = chunk ((round . sqrt) ((fromIntegral . length) forest)) forest
    _ <- case chunkedForest of
        Left str -> print $ "couldn't pretty print the trees, the chunk function couldn't make the sublists: " ++ str
        Right cf -> mapM_ (print . reverse) (reverse cf)
    let visibleTrees = getVisibleTrees forest
    print $ "# visible trees:" ++ length visibleTrees

data Tree = Tree {
    tHeight :: Int
    , tRow :: Int
    , tCol :: Int
    , hiddenDirs :: [Direction]
    }

data Direction = E | N | W | S

directions :: [Direction]
directions = [E, N, W, S]

numDirections :: Int
numDirections = length directions

instance Show Tree where
    show (Tree h r c _) = "(h:" ++ show h ++ " r:" ++ show r ++ " c:" ++ show c ++ ")"

mkTree :: Int -> Int -> Int -> Tree
mkTree th r c = Tree { tHeight = th, tRow = r, tCol = c }

-- Dim 1 is a row, left to right from the left edge
-- Dim 2 is a column, up to down from the top edge
createGrid :: [String] -> [Tree]
createGrid [] = []
createGrid inputs = createGrid' inputs 1 []

createGrid' :: [String] -> Int -> [Tree] -> [Tree]
createGrid' [] _ acc = acc
-- https://stackoverflow.com/questions/49144465/index-of-current-element-in-haskell-list-comprehension
createGrid' (x:xs) row acc = createGrid' xs (row + 1) (acc ++ [ mkTree h row idx | (idx, h) <- zip [1..] treeHeights])
    where
        treeHeights = map digitToInt x

getVisibleTrees :: [Tree] -> [Tree]
getVisibleTrees trees = getVisibleTrees' (reverse $ sort trees)

-- requires the list of trees to already by sorted by height
getVisibleTrees' :: [Tree] -> [Tree]
getVisibleTrees' [] = [] 
getVisibleTrees' allTrees@(x:xs) = do
    --let markHiddenFuncs = [ markFilterForDirec filterHeight treeRow treeCol d | d <- directions]
    let couldBeHidden = filter (\(Tree _ r c _) -> treeRow == r || treeCol == c) xs
    let markHiddenResults = markFilterForDirecs filterHeight treeRow treeCol couldBeHidden
    x:getVisibleTrees' filterRes
    where
        --recursiveResult = (getVisibleTrees' (map (filterForDirec tRow tCol) x))
        filterHeight = tHeight tallestTree
        treeRow = tRow tallestTree
        treeCol = tCol tallestTree
        tallestTree = head allTrees

markFilterForDirecs :: Tree -> [Tree] -> [Tree]
markFilterForDirecs tree trees = do
    -- filter looking from North to South
    map (markTree tree) trees
    where
        removeTotallyHidden = filter (\(Tree _ _ _ hDirs) -> length hDirs < numDirections) trees

markTree :: Tree -> Tree -> Tree
markTree tallestTree@(Tree h r c hideDirs) compareTree@(Tree ch cr cc cHide) = 
    where
        markNorth = (\(Tree _ r c _) -> c == tallTreeCol && r > tallTreeRow)
        -- looking S to N
        markSouth -> (\(Tree _ r c _) -> c == tallTreeCol && r < tallTreeRow)
        -- looking E to W
        markEast -> (\(Tree _ r c _) -> r == tallTreeRow && c > tallTreeCol)
        -- looking W to E
        markWest -> (\(Tree _ r c _) -> r == tallTreeRow && c < tallTreeCol)
