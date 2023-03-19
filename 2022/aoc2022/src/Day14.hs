module Day14
    --(
    --run
    --)
    where

import System.IO.Strict as ST (
    readFile
    )

import Data.List (
    find
    )

import Data.Maybe (
    isJust
    )

import Debug.Trace (
    trace
    )

import Text.Parsec (
    sepBy
    , sepBy1
    , Parsec
    , digit
    , char
    , many1
    , spaces
    , endOfLine
    , many
    , string
    , runParser
    , eof
    , skipMany
    , between
    , try
    )

data Location = Location Int Int deriving (Show)

--data Sand = Sand { settled::Bool, location::Location }

newtype RockPath = RockPath [Location] deriving Show
newtype Sand = Sand { location::Location } deriving (
    Show
    )

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
            --part2 rockpaths
            --print "part 2"
            --let floorY = 2 + maximum (map (\(Location _ y) -> y) (rockPathsToLocations rockpaths))
            --print $ "part2 floorY:" ++ show floorY
            --let part2Result = runSimulationPart2 floorY (Simulation (rockPathsToLocations rockpaths) [] (Location 500 0))
            --print $ "num of sand:" ++ show (length (sand part2Result))
part1 :: [RockPath] -> IO()
part1 rockpaths = do
    let rockpathLocations = rockPathsToLocations rockpaths
    let simResult = runSimulation $ Simulation rockpathLocations [] (Location 500 0)
    print "part1 result:"
    --print simResult
    --answer 618
    print $ "num of sand:" ++ show (length (sand simResult))

part2 :: [RockPath] -> IO()
part2 rockpaths = do
    print "part 2"
    let rockPathLocations = rockPathsToLocations rockpaths
    let floorY = 2 + maximum (map (\(Location _ y) -> y) rockPathLocations)
    print $ "part2 floorY:" ++ show floorY
    let part2Result = runSimulationPart2 floorY (Simulation rockPathLocations [] (Location 500 0))
    print $ "num of sand:" ++ show (length (sand part2Result))

data Simulation = Simulation { rPaths::[Location], sand::[Sand], flowPoint::Location }
    deriving (Show)

runSimulation :: Simulation -> Simulation
-- generate first unit of sand
--runSimulation s@(Simulation _ [] fp) = runSimulation $ s { sand=[Sand fp] }
runSimulation sim@(Simulation rockpaths _ _) =
    let lowestRockPath = maximum (map (\(Location _ rpy) -> rpy) rockpaths) in
        go lowestRockPath sim
    where
    go :: Int -> Simulation -> Simulation
    go lrp s@(Simulation _ [] fp) = go lrp $ s { sand=[Sand fp] }
    go lrp (Simulation rp sandList@(_:sands) fp) =
        --case trace (show gravityResult) gravityResult of
        case gravityResult of
            Settled _ -> go lrp (Simulation rp (Sand fp:sandList) fp)
            NewLocation newSand -> go lrp (Simulation rp (newSand:sands) fp)
            Abyss -> Simulation rp sands fp
        where
            gravityResult = gravity isAbyss isSettled sandList rp
            isAbyss (Location _ y) = y >= lrp -- maximum (y:map (\(Location _ rpy) -> rpy) rp) == y
            isSettled _ = False

-- now takes an integer that represents the y coordinate of the bottom
-- of the cave
runSimulationPart2 :: Int -> Simulation -> Simulation
runSimulationPart2 floorY s@(Simulation _ [] fp) = runSimulationPart2 floorY (s { sand=[Sand fp] })
runSimulationPart2 floorY s@(Simulation rp sandList@(_:sands) fp@(Location fpX fpY)) =
    --case trace (show gravityResult) gravityResult of
    case gravityResult of
        NewLocation newSand -> runSimulationPart2 floorY (s { sand=newSand:sands })
        Settled (Sand (Location x y)) ->
            if x == fpX && y == fpY
            -- the sand blocked the input source, so the simulation is over
            then s { sand=sandList }
            else runSimulationPart2 floorY newSimulation
                where
                    newSimulation = s { rPaths=rp, sand=Sand fp:sandList, flowPoint=fp }
        Abyss -> trace "sand fell into the abyss in part2, when it should be impossible" s


    where
        gravityResult = gravity isAbyss isSettled sandList rp
        isAbyss _ = False
        isSettled (Location _ ly) = ly == floorY - 1

data FallResult = NewLocation Sand | Settled Sand | Abyss deriving (Show)

gravity :: (Location -> Bool) -> (Location -> Bool) -> [Sand] -> [Location] -> FallResult
gravity _ _ [] _ = Abyss
gravity isAbyss isSettled (s@(Sand sl@(Location x y)):sands) rp
    | isSettled sl = Settled s
    | isAbyss sl = Abyss
    | not (checkOccupied down) = NewLocation $ Sand down
    | not (checkOccupied downL) = NewLocation $ Sand downL
    | not (checkOccupied downR) = NewLocation $ Sand downR
    | otherwise = Settled s
    where
        checkOccupied = isOccupied usedLocs
        down = Location x (y + 1)
        downL = Location (x - 1) (y + 1)
        downR = Location (x + 1) (y + 1)
        usedLocs = getUsedLocations sands rp

getUsedLocations :: [Sand] -> [Location] -> [Location]
getUsedLocations s rp = sandLocs++rp
    where
        sandLocs = map (\(Sand l) -> l) s

rockPathsToLocations :: [RockPath] -> [Location]
rockPathsToLocations = concatMap (\(RockPath l) -> l)

-- now accepts the y coordinate for the floor of the cavern
-- if sand y does not equal floor y, calls isOccupied as it was from part 1
--isOccupiedWithFloor :: Int -> [Location] -> Location -> Bool
--isOccupiedWithFloor floorY usedLocs (Location x y) =
--    y >= floorY - 1 || isOccupied usedLocs (Location x y)

isOccupied :: [Location] -> Location -> Bool
isOccupied [] _ = False
isOccupied usedLocs (Location x y) =
    isJust $ find (\(Location ox oy) -> x == ox && y == oy) usedLocs
    --let sameX = filter (\(Location ox _) -> x == ox) usedLocs
    --    otherYs = map (\(Location _ oy) -> oy) sameX
    --    in y `elem` otherYs

generateRockPath :: [(Int, Int)] -> RockPath
generateRockPath [] = RockPath []
generateRockPath pathPoints =
    RockPath $ foldl generateIntermediatePoints [] pathPoints

generateIntermediatePoints :: [Location] -> (Int, Int) -> [Location]
generateIntermediatePoints [] (ex, ey) = [Location ex ey]
generateIntermediatePoints prevPoints@(_:_) (ex, ey) = let Location sx sy = last prevPoints in
    if sx /= ex then prevPoints ++ [ Location x sy | x <- generatePointsBetween sx ex ]
    --if sx /= ex then prevPoints ++ drop 1 [ Location x sy | x <- [sx..ex] ]
    else if sy /= ey then prevPoints ++ [ Location sx y | y <- generatePointsBetween sy ey ]
    else []


generatePointsBetween :: Int -> Int -> [Int]
generatePointsBetween n1 n2 = reverse $ go [] n1 n2
    where
        go nums num1 num2
            | num1 < num2 = go ((num1 + 1):nums) (num1 + 1) num2
            | num1 > num2 = go ((num1 -1):nums) (num1 - 1) num2
            | otherwise = nums



comma :: Parsec String () Char
comma = char ','

digits :: Parsec String () Int
digits = read <$> many1 digit

plainWhitespace :: Parsec String () Char
plainWhitespace = char ' '

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
