module Day17 where

import Parsing (
    integer
    )

import Debug.Trace

import Text.Parsec

import qualified Data.Map as M

import qualified Data.List as L

data Jet = JLeft | JRight deriving (Eq)
instance Show Jet where
    show JLeft = "<"
    show JRight = ">"

-- the board is 7 units wide, and there is no ceiling
boardWidth :: Int
boardWidth = 7

-- two units away from the left edge 0 - 1 - ####
startPlacementLeftEdge :: Int
startPlacementLeftEdge = 2

-- spawns three units above the highest, settled rock
-- or the floor, if there are no other rocks (the initial state)
-- it says the bottom edge of the element must spawn 3 spaces away
-- from the floor or the highest rock that has already settled, so we need to
-- add four
spawnDistanceFromTopItem :: Int
spawnDistanceFromTopItem = 4

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
plusSign sx sy =
    [(sx + dx, horizBarY) | dx <- [0..2]]
    ++ [(vertBarX, sy), (vertBarX, sy + 2)]
    where
        horizBarY = sy + 1
        vertBarX = sx + 1

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
    [(distFromLeftEdge + dx, y) | dx <- [0..2]] ++ [(verticalPartX, y + 1), (verticalPartX, y + 2)]


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

-- it feels kinda bad to have the key and the value be the same thing
-- but I think Data.Map is faster than Data.Set, and when I want to filter the list
-- I can't use the data in the keys so, I'm going to try changing to this map type
type SettledPieces = M.Map Location Location

data SimState = SimState {
    fallingPieces :: [Location]
    , settledPieces :: SettledPieces
    , towerHeight :: Int
     -- hold the highest Y value for x values 0-6
     -- that way, I can throw away all of the rocks below the minimum Y value
     -- of that list (it's impossible for a new piece to fall below that point)
    , fileHeights :: M.Map Int Int
    , rocksToProcess :: Int
    , rocksProcessed :: Int
    , jets :: [(Jet, Int)]
    , piecesPattern :: [LabeledPlacePiece]
    , stage :: Stage
    }
instance Show SimState where
    show ss =
        "falling:" ++ show (fallingPieces ss)
        ++ "tower height:" ++ show (towerHeight ss)
        ++ "file heights:" ++ show (fileHeights ss)
        ++ " num settled:" ++ show (length (settledPieces ss))
        ++ " next jet:" ++ show (head (jets ss))
        ++ " next piece:" ++ show (head (piecesPattern ss))
        ++ " next stage:" ++ show (stage ss)
        ++ " rocks done/total:" ++ show (rocksProcessed ss) ++ "/" ++ show (rocksToProcess ss)

data Stage = DoJet | DoGravity deriving (Show)

gravity :: Int
gravity = 1

initSimState :: Int -> Int -> [(Jet, Int)] -> SimState
initSimState rocks numColumns js = SimState {
    fallingPieces=[]
    , settledPieces=M.empty
    , towerHeight=0
    , fileHeights=M.fromList [(x, 0) | x <- [0..numColumns-1]]
    , rocksToProcess=rocks
    , rocksProcessed=0
    , jets=cycle js -- the jets that are cycled as a piece is falling
    , stage=DoJet
    , piecesPattern=cycle [
        LabeledPlacePiece horizBar "horizBar"
        , LabeledPlacePiece plusSign "plusSign"
        , LabeledPlacePiece backwardsL "backwardsL"
        , LabeledPlacePiece verticalBar "verticalBar"
        , LabeledPlacePiece box "box"
        --LabeledPlacePiece backwardsL "backwardsL"
        ]
    }

doParse :: String -> Either ParseError (Int, Int, [Jet])
doParse = runParser Day17.parse () ""

-- part 1 3065
run :: String -> IO ()
run input = do
    print input
    case doParse input of
        Left e -> print e
        Right parseSuccess@(numRocks, throwAwayRockLimit, parsedJets) -> do
            --print $ "parse success:" ++ show parseSuccess
            --let numRocks = 2022
            let jetsWithId = zip parsedJets ([0..] :: [Int])
            let startSimState = initSimState numRocks boardWidth jetsWithId
                simResult = runSim startSimState throwAwayRockLimit
            let debugSettledPieces = settledPieces simResult
            --print $ "settledPieces" ++ show debugSettledPieces
            --putStrLn (printLocations debugSettledPieces)
            print $ "throwAwayRockLimit:" ++ show throwAwayRockLimit
            print $ "start sim state:" ++ show startSimState
            print $ "end sim state " ++ show simResult
            print $ "tower height:" ++ show (towerHeight simResult)

printLocations :: SettledPieces -> String
printLocations locs =
    if M.null locs
    then "no locations to print"
    else L.intercalate "\n" (reverse (go (map fst (M.toList locs)) 1 []))
    where
        go :: [Location] -> Int -> [String] -> [String]
        go goLocs currY acc = let rocksForY = filter (\(_, y) -> y == currY) goLocs in
            --if null (trace (show rocksForY) rocksForY)
            if null rocksForY
            then acc
            else
                go goLocs (currY + 1) (acc ++ [newStrings])
            where
                newStrings = [ if (atX, currY) `elem` goLocs then '#' else '.' | atX <- [0..(boardWidth - 1)]]

runSim :: SimState -> Int -> SimState
runSim ss throwAwayRockLimit
    -- | doneRocks == totalRocks = trace (show doneRocks) ss
    | doneRocks == totalRocks = ss
    | null fp = let nextX = startPlacementLeftEdge
                    nextY = (towerH + spawnDistanceFromTopItem)
                    nextPiece = genNextPiece nextX nextY
                in
                --trace ("=====================\ntowerHeight:" ++ show towerH ++ " new piece spawn origin:" ++ show (nextX, nextY) ++ " new piece locations" ++ show nextPiece)
                runSim  (ss {
                    fallingPieces=nextPiece
                    }) throwAwayRockLimit
    | otherwise = case thisStage of
        DoJet ->
            let nextLocations = map (applyJet thisJet) fp in
                -- if an illegal state would occur as a result of the rock being
                -- blown by the jet stream, then we continue the simulation without
                -- moving the rock, and skip to the gravity phase
                --trace ("jet:" ++ show thisJet)
                (
                let doneRocksMsg = "doneRocks:" ++ show doneRocks
                    currentLocsMsg = " currentLocs:" ++ show fp
                    nextLocsMsg = " next locs:" ++ show nextLocations
                    afterJetMsg = " afterJet:" ++ show thisJet
                in
                --if or (map isIllegal (trace (doneRocksMsg ++ currentLocsMsg ++ nextLocsMsg ++ afterJetMsg) nextLocations) ++ [locationsConflict nextLocations settled])
                if or (map isIllegal nextLocations ++ [locationsConflict nextLocations settled])
                then runSim (ss { stage=DoGravity, jets=nextJets }) throwAwayRockLimit
                else runSim (ss {
                    fallingPieces=nextLocations
                    , stage=DoGravity
                    , jets=nextJets
                    }) throwAwayRockLimit 
                )
        DoGravity ->
            --trace "gravity"
            (let nextLocations = map (applyGravity gravity) fp in
                --if any (isDoneFalling floorY settled) (trace ("nextLocs gravity:" ++ show nextLocations) nextLocations)
                if any (isDoneFalling floorY settled) nextLocations
                then
                    let stoppedDebugMessage = ("stopped at:" ++ show fp)
                        newSettledPieces = M.union (throwAwayCompletelyCoveredPieces settled newFileHeights) (M.fromList (zip fp fp))
                        -- an optimization to avoid calculating the maximum of the entire list of rocks was needed
                        -- it should be enough to take the max of the newly settled rocks and the previous tower height
                        -- that was saved with the simulation state
                        newTowerHeight = maximum (towerH:map snd fp)
                        newFileHeights = updateFileHeights fp (fileHeights ss)
                        printNewTowerHeight = ("new tower height:" ++ show newTowerHeight)
                    in
                        -- add the newly settled pieces to the list, generate a new piece, switch to jet stage (each piece starts off at the jet stage, per the requirement)
                        runSim (ss {
                                settledPieces=newSettledPieces
                                , stage=DoJet
                                , rocksProcessed=doneRocks + 1
                                , fallingPieces=[]
                                , piecesPattern=nextPiecesPattern
                                , towerHeight=newTowerHeight
                                , fileHeights=newFileHeights
                                --, towerHeight=trace printNewTowerHeight newTowerHeight
                                }) throwAwayRockLimit
                -- movement succeeded, continue simulation with the new locations still falling, switch to jet stage
                else runSim (ss {
                    stage=DoJet
                    , fallingPieces=nextLocations
                    }) throwAwayRockLimit
            )
    where
        fp = fallingPieces ss
        towerH = towerHeight ss
        settled = settledPieces ss
        doneRocks = rocksProcessed ss
        totalRocks = rocksToProcess ss
        (thisJet, _) = head (jets ss)
        nextJets = drop 1 (jets ss)
        thisStage = stage ss
        genNextPiece = (getF . head) piecesP
        nextPiecesPattern = drop 1 piecesP
        piecesP = piecesPattern ss

-- I think this is duplicated with the 'fall into another rock do to gravity'
-- check code in isDoneFalling
locationsConflict :: [Location] -> SettledPieces -> Bool
locationsConflict locs doneLocs = any (flip M.member doneLocs) locs

-- the pieces who's y coordinate is less than the minimum of the maximum heights
-- of each vertical file, those pieces can be safely removed from the list because
-- it is impossible for them to collide with the newly falling pieces
-- we need to throw them away because saving all the pieces is a massive space leak
throwAwayCompletelyCoveredPieces :: SettledPieces -> M.Map Int Int -> SettledPieces
throwAwayCompletelyCoveredPieces alreadySettled heightsPerFile =
    -- this check could be expensive so I don't think it's good to run it every update
    -- on inputs of under 10000 the space will not become an issue
    if length alreadySettled > 10000
    then let minimumCompletelyCoveredLine = minimum heightsPerFile in
        -- needs to be greater than or equal
        M.filter (\(_, y) -> y >= minimumCompletelyCoveredLine) alreadySettled
    else alreadySettled

updateFileHeights :: [Location] -> M.Map Int Int -> M.Map Int Int
updateFileHeights [] prevHeights = prevHeights
updateFileHeights ((x, y):rs) prevHeights =
    case M.lookup x prevHeights of
        Just prevHeight -> updateFileHeights rs (M.insert x (max prevHeight y) prevHeights)
        Nothing -> trace ("updateFileHeights failed to update file height:" ++ show x ++ " this should be impossible") prevHeights

isIllegal :: Location -> Bool
isIllegal (x, _) = x < 0 || x > boardWidth - 1

applyJet :: Jet -> Location -> Location
applyJet JLeft (x, y) = (x - 1, y)
applyJet JRight (x, y) = (x + 1, y)

applyGravity :: Int -> Location -> Location
applyGravity gravityAmount (x, y) = (x, y - gravityAmount)

isDoneFalling :: Int -> SettledPieces -> Location -> Bool
isDoneFalling floorHeight settledRocks checkLoc@(_, y) =
    let result = conflicts || hitsFloor in
        --if result then trace debugMessage result else result
        result
    where
        conflicts = M.member checkLoc settledRocks
        hitsFloor = y == floorHeight
        printLoc = show checkLoc
        debugMessage = "is done falling because " ++ if hitsFloor then "hits floor" else "hits other rock" ++ ":" ++ printLoc

parseLeft :: Parsec String () Jet
parseLeft = char '<' >> return JLeft

parseRight :: Parsec String () Jet
parseRight = char '>' >> return JRight

parse :: Parsec String () (Int, Int, [Jet])
parse = do
    numRocks <- integer <* endOfLine
    throwAwayRockLimit <- integer <* endOfLine
    parsedJets <- many (parseLeft <|> parseRight) <* endOfLine <* eof
    return (numRocks, throwAwayRockLimit, parsedJets)
