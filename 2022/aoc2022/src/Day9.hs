module Day9 (
    run
    ) where

import Prelude hiding (
    Left
    , Right
    )

import Debug.Trace

import System.Exit (
    exitFailure
    )

import Data.Set
import Data.Maybe (
    mapMaybe
    , Maybe(..)
    , fromJust
    )
import Data.Char (
    digitToInt
    )

run :: [String] -> IO ()
run inputLines = do
    print inputLines
    let motions = mapMaybe lineToMotion inputLines
    print motions
    let simResult@(rope, tailPositions) = runSimulation (Rope (Head (0, 0)) (Tail (0, 0))) (fromList []) motions
    putStrLn $ "simResult:" ++ show simResult
    putStrLn $ "final rope position:" ++ show rope ++ "# of unique tail positions:" ++ show (numTailPositions tailPositions)

newtype Head = Head Position deriving (Show)
newtype Tail = Tail Position deriving (Show)
data Rope = Rope Head Tail deriving (Show)

type Position = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Show)
directionFromChar :: Char -> Maybe Direction
directionFromChar c = case c of
    'U' -> Just Up
    'D' -> Just Down
    'L' -> Just Left
    'R' -> Just Right
    _ -> Nothing 

-- +1 for the starting position
numTailPositions :: Set Position -> Int
numTailPositions = (+1) . length

lineToMotion :: String -> Maybe (Direction, Int)
lineToMotion [] = Nothing
lineToMotion [char, ' ', num] = let newDirection = directionFromChar char in
    case newDirection of
        Just _ -> Just (fromJust newDirection, digitToInt num)
        Nothing -> Nothing
lineToMotion _ = Nothing

runSimulation :: Rope -> Set Position -> [(Direction, Int)] -> (Rope, Set Position)
runSimulation rope positions [] = (rope, positions)
runSimulation rope@(Rope head@(Head (hx, hy)) tail@(Tail (tx, ty))) positions (mt:mts) =
    let (newRope, tailPositions) = runMotion head tail positions mt in
    runSimulation newRope tailPositions mts

runMotion :: Head -> Tail -> Set Position -> (Direction, Int) -> (Rope, Set Position)
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (_, 0) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
--runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) =
    let newHeadPos = updatePos (hx, hy) (direction, 1) in
    let distance = getDirectDistance newHeadPos (tx, ty) in
    if trace (show distance) distance == 1
    then runMotion (Head newHeadPos) (Tail (tx, ty)) positionAcc (direction, magnitude - 1)
    else
        if distance >= 2
        then runMotion (Head newHeadPos) (Tail (hx, hy)) (insert (hx, hy) positionAcc) (direction, magnitude - 1)
        else runMotion (Head newHeadPos) (Tail (tx, ty)) positionAcc (direction, magnitude - 1)

updatePos :: Position -> (Direction, Int) -> Position
updatePos (x, y) motion = case motion of
    (Left, num) -> (x - num, y)
    (Right, num) -> (x + num, y)
    (Up, num) -> (x, y + num)
    (Down, num) -> (x, y - num)

getDistance :: Position -> Position -> (Int, Int)
getDistance (hx, hy) (tx, ty) = (hx - tx, hy - ty)

getDirectDistance :: Position -> Position -> Float
getDirectDistance pos1 pos2 = let (distX, distY) = getDistance pos1 pos2 in sqrt $ (fromIntegral distX ** 2) + (fromIntegral distY ** 2)
