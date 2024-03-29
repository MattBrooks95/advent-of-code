module Day14
    --(
    --run
    --)
    where

import System.IO.Strict as ST (
    readFile
    )

--import qualified Data.Vector as V

import Data.Function (
    on
    )

import Data.List (
    find
    , groupBy
    , maximumBy
    , sortBy
    )

import qualified Data.Map as M

import Data.Maybe (
    isJust
    )

import Debug.Trace (
    trace
    )

import Text.Parsec (
    sepBy1
    , Parsec
    , endOfLine
    , many
    , string
    , runParser
    , eof
    , between
    )

import Parsing

--data Location = Location Int Int deriving (Show)

data Matter = Rock | Sand deriving Show

data Location = Location (Int, Int) Matter deriving Show

isSand :: Location -> Bool
isSand (Location _ mType) = case mType of
    Sand -> True
    _ -> False

--data Sand = Sand { settled::Bool, location::Location }

newtype RockPath = RockPath [Location] deriving Show
--newtype Sand = Sand { location::Location } deriving (
--    Show
--    )

run :: FilePath -> IO()
run fp = do
    fileContents <- ST.readFile fp
    --print fileContents
    print "day 14"
    case runParser parse () fp fileContents of
        Left parseError -> print parseError
        Right parsedRps -> do
            --mapM_ print parsedRps
            let rockpaths = map generateRockPath parsedRps
            --print rockpaths
            --let simResult = runSimulation $ Simulation rockpaths [] (Location 500 0)
            --print "part1 result:"
            --print simResult
            --answer 618
            --print $ "num of sand:" ++ show (length (sand simResult))
            part1 rockpaths
            print "part 2"
            part2 rockpaths
            --let floorY = 2 + maximum (map (\(Location _ y) -> y) (rockPathsToLocations rockpaths))
            --print $ "part2 floorY:" ++ show floorY
            --let part2Result = runSimulationPart2 floorY (Simulation (rockPathsToLocations rockpaths) [] (Location 500 0))
            --print $ "num of sand:" ++ show (length (sand part2Result))

part1 :: [RockPath] -> IO()
part1 rockpaths = do
    let rockpathLocations = rockPathsToLocations rockpaths
    let sortedByMaxIndex = sortBy (compare `on` hashLoc) rockpathLocations
    print $ "lowest 10:" ++ concatMap show (take 10 sortedByMaxIndex) ++ " highest 10:" ++ concatMap show (drop (length sortedByMaxIndex - 10) sortedByMaxIndex)
    let locMap = initMap rockpathLocations
    --print locsVec
    let simResult = runSimulation $ Simulation locMap (500, 0) (500, 0)
    print "part1 result:"
    --print simResult
    --answer 618
    --print $ "num of sand:" ++ show (length (currentSand simResult))
    print $ "num of sand:" ++ show (length (getSand (locations simResult)))

--getSand :: Locations -> Location
--getSand = concatMap getSandFromBucket

getSand :: M.Map (Int, Int) Location -> M.Map (Int, Int) Location
getSand = M.filter (\(Location _ mType) -> case mType of { Sand -> True; _ -> False; })

getSandFromBucket :: [Location] -> [Location]
getSandFromBucket = filter isSand

initMap :: [Location] -> M.Map (Int, Int) Location
initMap locs = M.fromList (map (\loc@(Location coords _) -> (coords, loc)) locs)
    --let
    --    maxIndex = getMinMaxIndex locs
    --    groupedRocks = groupBy (\loc1 loc2 -> hashLoc loc1 == hashLoc loc2) locs
    --in
    --    V.replicate maxIndex [] V.// map bucketWithHash groupedRocks

bucketWithHash :: [Location] -> (Int, [Location])
bucketWithHash [] = (-1, [])
bucketWithHash bucket@(x:_) = (hashLoc x, bucket)

hashLoc :: Location -> Int
hashLoc (Location (x, y) _) = x * y

--getMinMaxIndex :: [Location] -> Int
--getMinMaxIndex locs = let (mX, mY) = go (0, 0) locs in (mX + 1) * (mY + 1)
--    where
--        go acc [] = trace ("minMax:" ++ show acc) acc
--        go (maxX, maxY) (Location (x, y) _:ls) = go (max maxX x, max maxY y) ls

--26032 too low
--26358, I was checking against the floor Y value minus 1, instead of the floor Y value...
part2 :: [RockPath] -> IO()
part2 rockpaths = do
    print "part 2"
    let rockpathLocations = rockPathsToLocations rockpaths
    let locsVec = initMap rockpathLocations
    let floorY = 1 + maximum (map (\(Location (_, y) _) -> y) rockpathLocations)
    print $ "part2 floorY:" ++ show floorY
    let part2Result = runSimulationPart2 floorY (Simulation locsVec (500, 0) (500, 0))
    print $ "num of sand:" ++ show (length (getSand (locations part2Result)))
    case M.lookup (500, 0) (locations part2Result) of
        Nothing -> print "the sand that is supposed to be blocking the input isn't in the list"
        Just stopped -> print $ "sand that filled the hole:" ++ show stopped
    --print $ "num of sand:" ++ show (length (sand part2Result))

data Simulation = Simulation { locations::Locations, currentSand::(Int, Int), flowPoint::(Int, Int) }
    deriving (Show)

runSimulation :: Simulation -> Simulation
-- generate first unit of sand
--runSimulation s@(Simulation _ [] fp) = runSimulation $ s { sand=[Sand fp] }
runSimulation sim@(Simulation startLocs _ fp) =
    let lowestRockPath = trace (show $ maxYOfLocs startLocs) (maxYOfLocs startLocs) in
        go lowestRockPath sim
    where
    go :: Int -> Simulation -> Simulation
    go lrp (Simulation locs currSand flowp) =
        --case trace (show gravityResult) gravityResult of
        case gravityResult of
            Settled settledSand ->
                go lrp (Simulation (insert locs (Location settledSand Sand)) fp fp)
            NewLocation newSand -> go lrp (Simulation locs newSand flowp)
            Abyss -> Simulation locs currSand fp
        where
            gravityResult = gravity isAbyss isSettled locs currSand
            isAbyss (_, y) = y >= lrp -- maximum (y:map (\(Location _ rpy) -> rpy) rp) == y
            isSettled _ = False

maxYOfLocs :: Locations -> Int
maxYOfLocs locs =
    let (Location (_, y) _)  = maximumBy (compare `on` getY) asList in y
    where
        asList = map (\((_, _), loc) -> loc) mapAsList
        mapAsList = M.toList locs
    --if null locs then -1
    --else let maxYBucket = M.maximumBy (compare `on` maxYLocation) locs in
    --    maxYLocation maxYBucket

getY :: Location -> Int
getY (Location (_, y) _) = y

maxYLocation :: [Location] -> Int
maxYLocation locs =
    if null locs
    then -1
    else let (Location (_, y) _) = maximumBy (compare `on` getY) locs in y

-- now takes an integer that represents the y coordinate of the bottom
-- of the cave
runSimulationPart2 :: Int -> Simulation -> Simulation
--runSimulationPart2 floorY s@(Simulation _  fp) = runSimulationPart2 floorY (s { currentSand=fp })
runSimulationPart2 floorY s@(Simulation locs sand fp@(fpX, fpY)) =
    --case trace (show gravityResult) gravityResult of
    case gravityResult of
        --NewLocation newSand -> runSimulationPart2 floorY (s { currentSand= trace("new sand:" ++ show newSand) newSand })
        NewLocation newSand -> runSimulationPart2 floorY (s { currentSand=newSand })
        Settled settledSand@(x, y) ->
            if x == fpX && y == fpY
            -- the sand blocked the input source, so the simulation is over
            then newSimulation
            else runSimulationPart2 floorY newSimulation
                where
                    newSimulation = s { locations=insert locs (Location settledSand Sand), currentSand=fp }
        Abyss -> trace "sand fell into the abyss in part2, when it should be impossible" s


    where
        gravityResult = gravity isAbyss isSettled locs sand
        isAbyss _ = False
        isSettled (_, ly) = ly == floorY

data FallResult = NewLocation (Int, Int) | Settled (Int, Int) | Abyss deriving (Show)

gravity :: ((Int, Int) -> Bool) -> ((Int, Int) -> Bool) -> Locations -> (Int, Int) -> FallResult
gravity isAbyss isSettled locs currSand@(cx, cy)
    | isSettled currSand = Settled currSand
    | isAbyss currSand = Abyss
    | not (checkOccupied down) = NewLocation down
    | not (checkOccupied downL) = NewLocation downL
    | not (checkOccupied downR) = NewLocation downR
    | otherwise = Settled currSand
    where
        checkOccupied = isOccupied locs
        down = (cx, cy + 1)
        downL = (cx - 1, cy + 1)
        downR = (cx + 1, cy + 1)
        --usedLocs = getUsedLocations sands rp

--getUsedLocations :: [Sand] -> [Location] -> [Location]
--getUsedLocations s rp = sandLocs++rp
--    where
--        sandLocs = map (\(Sand l) -> l) s

rockPathsToLocations :: [RockPath] -> [Location]
rockPathsToLocations = concatMap (\(RockPath l) -> l)

-- now accepts the y coordinate for the floor of the cavern
-- if sand y does not equal floor y, calls isOccupied as it was from part 1
--isOccupiedWithFloor :: Int -> [Location] -> Location -> Bool
--isOccupiedWithFloor floorY usedLocs (Location x y) =
--    y >= floorY - 1 || isOccupied usedLocs (Location x y)

type Locations = M.Map (Int, Int) Location

insert :: Locations -> Location -> Locations
insert locs newLoc@(Location (x, y) _) = M.insert (x, y) newLoc locs

isOccupied :: Locations -> (Int, Int) -> Bool
isOccupied usedLocs (x, y) =
    isJust $ getLoc usedLocs (x, y)

getLoc :: Locations -> (Int, Int) -> Maybe Location
getLoc locs (x, y) = M.lookup (x, y) locs

    --isJust $ find (\(Location ox oy) -> x == ox && y == oy) usedLocs
    --let sameX = filter (\(Location ox _) -> x == ox) usedLocs
    --    otherYs = map (\(Location _ oy) -> oy) sameX
    --    in y `elem` otherYs

generateRockPath :: [(Int, Int)] -> RockPath
generateRockPath [] = RockPath []
generateRockPath pathPoints =
    RockPath $ foldl generateIntermediatePoints [] pathPoints

generateIntermediatePoints :: [Location] -> (Int, Int) -> [Location]
generateIntermediatePoints [] (ex, ey) = [Location (ex, ey) Rock]
generateIntermediatePoints prevPoints@(_:_) (ex, ey) = let Location (sx, sy) _ = last prevPoints in
    if sx /= ex then prevPoints ++ [ Location (x, sy) Rock | x <- generatePointsBetween sx ex ]
    --if sx /= ex then prevPoints ++ drop 1 [ Location x sy | x <- [sx..ex] ]
    else if sy /= ey then prevPoints ++ [ Location (sx, y) Rock | y <- generatePointsBetween sy ey ]
    else []


generatePointsBetween :: Int -> Int -> [Int]
generatePointsBetween n1 n2 = reverse $ go [] n1 n2
    where
        go nums num1 num2
            | num1 < num2 = go ((num1 + 1):nums) (num1 + 1) num2
            | num1 > num2 = go ((num1 -1):nums) (num1 - 1) num2
            | otherwise = nums



numberPair :: Parsec String () (Int, Int)
numberPair = do
    digit1 <- digits
    _ <- comma
    digit2 <- digits
    return (digit1, digit2)

sepArrow :: Parsec String () String
sepArrow = between ws ws (string "->")
    where
        ws = many plainWhitespace

parseRockPath :: Parsec String () [(Int, Int)]
parseRockPath = numberPair `sepBy1` sepArrow

parse :: Parsec String () [[(Int, Int)]]
parse = many (parseRockPath <* endOfLine) <* eof
