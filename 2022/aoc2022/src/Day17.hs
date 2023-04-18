module Day17 where

import Text.Parsec

import qualified Data.Map as M

data Jet = JLeft | JRight deriving Show

-- the board is 7 units wide, and there is no ceiling
boardWidth :: Int
boardWidth = 7

-- two units away from the left edge 0 - 1 - ####
startPlacementLeftEdge :: Integer
startPlacementLeftEdge = 2

-- spawns three units above the highest, settled rock
-- or the floor, if there are no other rocks (the initial state)
spawnDistanceFromTopItem :: Int
spawnDistanceFromTopItem = 3

floorY :: Int
floorY = -1

type Location = (Int, Int)

data SimState = SimState {
    fallingPieces :: [Location]
    , settledPieces :: M.Map Location ()
    , towerHeight :: Int
    , rocksToProcess :: Int
    , rocksProcessed :: Int
    , jets :: [Jet]
    , stage :: Stage
    }

data Stage = DoJet | DoGravity

gravity :: Int
gravity = 1

initSimState :: Int -> [Jet] -> SimState
initSimState rocks js = SimState {
    fallingPieces=[]
    , settledPieces=M.empty
    , towerHeight=0
    , rocksToProcess=rocks
    , rocksProcessed=0
    , jets=cycle js
    , stage=DoGravity
    }

run :: String -> IO ()
run input = do
    print input
    case runParser Day17.parse () "" input of
        Left e -> print e
        Right parseSuccess -> do
            print $ "parse success:" ++ show parseSuccess

runSim :: SimState -> SimState
runSim ss
    | doneRocks == totalRocks = ss
    | otherwise = case thisStage of
        DoJet ->
            let nextLocations = map (applyJet thisJet) fp in
                if or (map isIllegal nextLocations ++ [locationsConflict nextLocations settled])
                then ss { stage=DoGravity }
                else ss { fallingPieces=nextLocations, stage=DoGravity }
        DoGravity ->
            let nextLocations = map (applyGravity gravity) fp in
                if any (isDoneFalling floorY settled) nextLocations
                then ss -- add the newly settled pieces to the list, generate a new piece, switch to gravity stage
                else ss -- movement succeeded, continue simulation with the new locations still falling, switch to gravity stage

    where
        fp = fallingPieces ss
        towerH = towerHeight ss
        settled = settledPieces ss
        doneRocks = rocksProcessed ss
        totalRocks = rocksToProcess ss
        thisJet = head (jets ss)
        nextJets = drop 1 (jets ss)
        thisStage = stage ss

locationsConflict :: [Location] -> M.Map Location () -> Bool
locationsConflict locs doneLocs = any (flip M.member doneLocs) locs

isIllegal :: Location -> Bool
isIllegal (x, _) = x < 0 || x > boardWidth - 1

applyJet :: Jet -> Location -> Location
applyJet JLeft (x, y) = (x - 1, y)
applyJet JRight (x, y) = (x + 1, y)

applyGravity :: Int -> Location -> Location
applyGravity gravityAmount (x, y) = (x, y - gravityAmount)

updatePieceLocations :: (Location -> Location) -> [Location] -> [Location]
updatePieceLocations f locs = map f locs

isDoneFalling :: Int -> M.Map Location () -> Location -> Bool
isDoneFalling floorHeight settledRocks checkLoc@(_, y) = 
    M.member checkLoc settledRocks || y == floorHeight + 1

parseLeft :: Parsec String () Jet
parseLeft = char '<' >> return JLeft

parseRight :: Parsec String () Jet
parseRight = char '>' >> return JRight

parse :: Parsec String () [Jet]
parse = many (parseLeft <|> parseRight) <* endOfLine <* eof
