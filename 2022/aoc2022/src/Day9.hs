module Day9
    --(
    --run 
    --)
    where 

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
    --print inputLines
    let motions = mapMaybe lineToMotion inputLines
    --print motions
    let simResult@(rope, tailPositions) = runSimulation (Rope (Head (0, 0)) (Tail (0, 0))) [] motions
    --putStrLn $ "simResult:" ++ show simResult
    --putStrLn $ "final rope position:" ++ show rope
    --putStrLn $ "tail positions: " ++ show (reverse tailPositions)
    --putStrLn $ "num tail positions: " ++ show (numTailPositions tailPositions)
    putStrLn $ "num unique tail positions: " ++ show (numUniqueTailPositions tailPositions)
    putStrLn "part2=============================="
    let partTwoRope = Part2Rope $ replicate 10 (0, 0)
    --print partTwoRope
    let (finalRope, prevRopes) = runSimulationPart2 partTwoRope [] motions
    --print $ "final rope:" ++ show finalRope
    --print $ "final rope length:" ++ (show . getLength) finalRope
    --print $ "previous ropes" ++ concat (replicate 20 "#")
    --mapM_ print prevRopes
    let allRopeTailPositions = map (last . getPositions) prevRopes
    print $ "num unique tail positions:" ++ show (length (fromList allRopeTailPositions))
    --let prevRopes = snd partTwoRopeSimResult
    --mapM_ print (reverse prevRopes)
    --let tailPositions = map (last . getPositions) prevRopes
    --print tailPositions
    --let uniqueTailPositions = fromList tailPositions
    --print uniqueTailPositions
    --print $ length uniqueTailPositions


newtype Head = Head Position deriving (Show)
newtype Tail = Tail Position deriving (Show)
data Rope = Rope Head Tail deriving (Show)

-- the part two rope is Head - segments 1 - 9
-- 9 is the tail
newtype Part2Rope = Part2Rope [Position]
    deriving (Show)

getPositions :: Part2Rope -> [Position]
getPositions (Part2Rope positions) = positions

getLength :: Part2Rope -> Int
getLength (Part2Rope list) = length list


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

-- create a set of the tail positions, then turn it back into a list and take
-- the length
numUniqueTailPositions :: [Position] -> Int
-- length should work for a set, so I didn't need to do this...
--numUniqueTailPositions tpos = numTailPositions $ toList $ fromList tpos
numUniqueTailPositions = length . fromList

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

runSimulationPart2 :: Part2Rope -> [Part2Rope] -> [(Direction, Int)] -> (Part2Rope, [Part2Rope])
runSimulationPart2 rope prevRopes [] = (rope, prevRopes)
runSimulationPart2 (Part2Rope rope) prevRopes (movement:movements) =
    let (newRope, intermediateRopeStates) = runMotionPart2 (rope, []) movement in
    runSimulationPart2 (Part2Rope newRope) (concat [prevRopes, [Part2Rope newRope], map Part2Rope intermediateRopeStates]) movements

runMotion :: Head -> Tail -> [Position] -> (Direction, Int) -> (Rope, [Position])
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (_, 0) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
--runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) = (Rope (Head (hx, hy)) (Tail (tx, ty)), positionAcc)
runMotion (Head (hx, hy)) (Tail (tx, ty)) positionAcc (direction, magnitude) =
    let newHeadPos = updatePos (hx, hy) (direction, 1)
        distance = getDirectDistance newHeadPos (tx, ty) in
    --if trace (show distance) distance == 1
    if distance == 1
    then runMotion (Head newHeadPos) (Tail (tx, ty)) ((tx, ty):positionAcc) (direction, magnitude - 1)
    else
        if distance >= 2
        then runMotion (Head newHeadPos) (Tail (hx, hy)) ((hx, hy):positionAcc) (direction, magnitude - 1)
        else runMotion (Head newHeadPos) (Tail (tx, ty)) ((tx, ty):positionAcc) (direction, magnitude - 1)

runMotionPart2 :: ([Position], [[Position]]) -> (Direction, Int) -> ([Position], [[Position]])
runMotionPart2 ([], _) _ = ([], [])
runMotionPart2 pos (_, 0) = pos
runMotionPart2 (head:remaining, prevStates) (direc, mag) =
    let newHead = updatePos head (direc, 1)
        newTails = foldl (\predecessors y -> predecessors++[tailFollowsHead (last predecessors) y] ) [newHead] remaining in
        let newRope = newTails in
            runMotionPart2 (newRope, newRope:prevStates) (direc, mag - 1)

tailFollowsHead :: Position -> Position -> Position
tailFollowsHead head tail =
    let distance = getDirectDistance head tail in
    if distance >= 2 then
        let xDistance = fst head - fst tail
            yDistance = snd head - snd tail
            applyDelta = getMovementToFollow xDistance yDistance
        in
            applyDelta tail
    else tail

getMovementToFollow :: Int -> Int -> (Position -> Position)
getMovementToFollow xDiff yDiff =
    if isDiagonalMovement xDiff yDiff then
        let xDelta = getDelta xDiff
            yDelta = getDelta yDiff
        in
            \(px, py) -> (px + xDelta, py + yDelta)
    else
        case xDiff of
            2 -> (\(px, py) -> (px + 1, py))
            -2 -> (\(px, py) -> (px - 1, py))
            _ -> case yDiff of
                2 -> \(px, py) -> (px, py + 1)
                -2 -> \(px, py) -> (px, py - 1)
                _ -> id -- this should never happen, if it's not a diagonal movement then the x or y diff will be 2

getDelta :: Int -> Int
getDelta diff = if abs diff == 2
    then (if diff > 0 then 1 else -1)
    else diff

isDiagonalMovement :: Int -> Int -> Bool
isDiagonalMovement xDiff yDiff = abs xDiff > 0 && abs yDiff > 0

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
