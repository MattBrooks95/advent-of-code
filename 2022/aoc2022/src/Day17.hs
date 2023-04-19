module Day17 where

import Debug.Trace

import Text.Parsec

import qualified Data.Map as M

import qualified Data.List as L

data Jet = JLeft | JRight deriving Show

-- the board is 7 units wide, and there is no ceiling
boardWidth :: Int
boardWidth = 7

-- two units away from the left edge 0 - 1 - ####
startPlacementLeftEdge :: Int
startPlacementLeftEdge = 2

-- spawns three units above the highest, settled rock
-- or the floor, if there are no other rocks (the initial state)
spawnDistanceFromTopItem :: Int
spawnDistanceFromTopItem = 3

floorY :: Int
floorY = 0

type Location = (Int, Int)

type PlacePiece = Int -> Int -> [Location]

-- ####
horizBar :: PlacePiece
horizBar distFromLeftEdge y = [ (x + distFromLeftEdge, y) | x <- [0..3]]

-- #
-- #
-- #
-- #
verticalBar :: PlacePiece
verticalBar distFromLeftEdge y = [ (distFromLeftEdge, y + dy) | dy <- [0..3] ]

--  #
-- ###
--  #
plusSign :: PlacePiece
plusSign distFromLeftEdge y =
    [(distFromLeftEdge + dx, horizBarY) | dx <- [0..2]]
    ++ [(vertBarX, y + dy) | dy <- [0..2]]
    where
        horizBarY = y + 2
        vertBarX = distFromLeftEdge + 1

-- ##
-- ##
box :: PlacePiece
box distFromLeftEdge y =
    [ (distFromLeftEdge, y)
    , (distFromLeftEdge, y + 1)
    , (distFromLeftEdge + 1, y)
    , (distFromLeftEdge + 1, y + 1)
    ]

--   #
--   #
-- ###
backwardsL :: PlacePiece
backwardsL distFromLeftEdge y = let verticalPartX = distFromLeftEdge + 2 in
    [(distFromLeftEdge + dx, y) | dx <- [0..3]] ++ [(verticalPartX, y + 1), (verticalPartX, y + 2)]


rocksPattern :: [PlacePiece]
rocksPattern = cycle [
    horizBar
    , plusSign
    , backwardsL
    , verticalBar
    , box
    ]

data LabeledPlacePiece = LabeledPlacePiece PlacePiece String
instance Show LabeledPlacePiece where
    show (LabeledPlacePiece _ s) = s

getF :: LabeledPlacePiece -> PlacePiece
getF (LabeledPlacePiece pp _) = pp

data SimState = SimState {
    fallingPieces :: [Location]
    , settledPieces :: M.Map Location ()
    , towerHeight :: Int
    , rocksToProcess :: Int
    , rocksProcessed :: Int
    , jets :: [Jet]
    , piecesPattern :: [LabeledPlacePiece]
    , stage :: Stage
    }
instance Show SimState where
    show ss =
        "falling:" ++ show (fallingPieces ss)
        ++ "tower height:" ++ show (towerHeight ss)
        ++ " num settled:" ++ show (length (settledPieces ss))
        ++ " next jet:" ++ show (head (jets ss))
        ++ " next piece:" ++ show (head (piecesPattern ss))
        ++ " next stage:" ++ show (stage ss)
        ++ " rocks done/total:" ++ show (rocksProcessed ss) ++ "/" ++ show (rocksToProcess ss)

data Stage = DoJet | DoGravity deriving (Show)

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
    , piecesPattern=cycle [
        LabeledPlacePiece horizBar "horizBar"
        , LabeledPlacePiece plusSign "plusSign"
        , LabeledPlacePiece backwardsL "backwardsL"
        , LabeledPlacePiece verticalBar "verticalBar"
        , LabeledPlacePiece box "box"
        ]
    }

run :: String -> IO ()
run input = do
    print input
    case runParser Day17.parse () "" input of
        Left e -> print e
        Right parseSuccess -> do
            print $ "parse success:" ++ show parseSuccess
            let startSimState = initSimState 2022 parseSuccess
                simResult = runSim startSimState
            print $ "start sim state:" ++ show startSimState
            print $ "end sim state:" ++ show simResult

runSim :: SimState -> SimState
runSim ss
    | null fp = let nextPiece = genNextPiece startPlacementLeftEdge (towerH + 3) in
        runSim $ ss { fallingPieces=nextPiece }
    | trace (show doneRocks) doneRocks == totalRocks = ss
    | otherwise = case thisStage of
        DoJet ->
            let nextLocations = map (applyJet thisJet) fp in
                if or (map isIllegal nextLocations ++ [locationsConflict nextLocations settled])
                then runSim $ ss { stage=DoGravity, jets=nextJets }
                else runSim $ ss {
                    fallingPieces=nextLocations
                    , stage=DoGravity
                    , jets=nextJets
                    }
        DoGravity ->
            let nextLocations = map (applyGravity gravity) fp in
                if any (isDoneFalling floorY settled) nextLocations
                then let newTowerHeight = maximum (map snd nextLocations) in
                    runSim $ ss { -- add the newly settled pieces to the list, generate a new piece, switch to jet stage (each piece starts off at the jet stage, per the requirement)
                            settledPieces=M.union (settledPieces ss) (M.fromList (zip nextLocations (repeat ())))
                            , stage=DoJet
                            , rocksProcessed=rocksProcessed ss + 1
                            , fallingPieces=[]
                            , piecesPattern=nextPiecesPattern
                            , towerHeight=newTowerHeight
                        }
                else runSim $ ss { -- movement succeeded, continue simulation with the new locations still falling, switch to jet stage
                    stage=DoJet
                    , fallingPieces=nextLocations
                    }

    where
        fp = fallingPieces ss
        towerH = towerHeight ss
        settled = settledPieces ss
        doneRocks = rocksProcessed ss
        totalRocks = rocksToProcess ss
        thisJet = head (jets ss)
        nextJets = drop 1 (jets ss)
        thisStage = stage ss
        genNextPiece = (getF . head) piecesP
        nextPiecesPattern = drop 1 piecesP
        piecesP = piecesPattern ss

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
