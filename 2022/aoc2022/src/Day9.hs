module Day9 (
    run 
    ) where 

import Prelude hiding (
    Left
    , Right
    , head
    , tail
    )

import qualified Prelude as P (
    head
    )

import Debug.Trace

import Data.Set (
    fromList
    , toList
    )
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
    let simResult@(rope, tailPositions) = runSimulation (Rope (Head (0, 0)) (Tail (0, 0))) [] motions
    putStrLn $ "simResult:" ++ show simResult
    putStrLn $ "final rope position:" ++ show rope
    putStrLn $ "tail positions: " ++ show (reverse tailPositions)
    putStrLn $ "num tail positions: " ++ show (numTailPositions tailPositions)
    putStrLn $ "num unique tail positions: " ++ show (numUniqueTailPositions tailPositions)

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
-- going to include redundant positions in the tail list, and then do the uniqueness check at the end
-- so no longer need to add 1
--numTailPositions :: [Position] -> Int
--numTailPositions = (+1) . length
numTailPositions :: [Position] -> Int
numTailPositions = length

numUniqueTailPositions :: [Position] -> Int
numUniqueTailPositions tpos = numTailPositions $ toList $ fromList tpos

lineToMotion :: String -> Maybe (Direction, Int)
lineToMotion [] = Nothing
-- this was a massive bug, the magnitudes weren't restricted to 0-9
--lineToMotion [char, ' ', num] = let newDirection = directionFromChar char in
--    case newDirection of
--        Just _ -> Just (fromJust newDirection, digitToInt num)
--        Nothing -> Nothing
--lineToMotion _ = Nothing
lineToMotion line =
    if length line < 3
    then Nothing
    else case directionFromChar $ P.head line of 
        Just d -> Just (d, read (drop 2 line) :: Int)
        Nothing -> Nothing

runSimulation :: Rope -> [Position] -> [(Direction, Int)] -> (Rope, [Position])
runSimulation rope positions [] = (rope, positions)
runSimulation (Rope head@(Head _) tail@(Tail _)) positions (mt:mts) =
    let (newRope, tailPositions) = runMotion head tail positions mt in
    runSimulation newRope tailPositions mts

runMotion :: Head -> Tail -> [Position] -> (Direction, Int) -> (Rope, [Position])
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (_, 0) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
--runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) =
    let newHeadPos = updatePos (hx, hy) (direction, 1) in
    let distance = getDirectDistance newHeadPos (tx, ty) in
    if trace (show distance) distance == 1
    then runMotion (Head newHeadPos) (Tail (tx, ty)) ((tx, ty):positionAcc) (direction, magnitude - 1)
    else
        if distance >= 2
        then runMotion (Head newHeadPos) (Tail (hx, hy)) ((hx, hy):positionAcc) (direction, magnitude - 1)
        else runMotion (Head newHeadPos) (Tail (tx, ty)) ((tx, ty):positionAcc) (direction, magnitude - 1)

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
