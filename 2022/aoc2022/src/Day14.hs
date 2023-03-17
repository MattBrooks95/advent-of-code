module Day14
    --(
    --run
    --)
    where

import System.IO.Strict as ST (
    readFile
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
    print fileContents
    print "day 14"
    case runParser parse () fp fileContents of
        Right parsedRps -> do
            mapM_ print parsedRps
            let rockpaths = map generateRockPath parsedRps
            print rockpaths
            let simResult = runSimulation $ Simulation rockpaths [] (Location 500 0)
            print "part1 result:"
            print simResult
        Left parseError -> print parseError

data Simulation = Simulation { rPaths::[RockPath], sand::[Sand], flowPoint::Location }
    deriving (Show)

runSimulation :: Simulation -> Simulation
-- generate first unit of sand
runSimulation s@(Simulation _ [] fp) = s { sand=[Sand fp] }
runSimulation (Simulation rp sandList@(_:sands) fp) =
    case gravity sandList rp of
        Settled _ -> runSimulation $ Simulation rp (Sand fp:sandList) fp
        NewLocation newSand -> runSimulation $ Simulation rp (newSand:sands) fp
        Abyss -> Simulation rp sands fp

data FallResult = NewLocation Sand | Settled Sand | Abyss

gravity :: [Sand] -> [RockPath] -> FallResult
gravity [] _ = Abyss
gravity (s@(Sand (Location x y)):sands) rp
    | checkOccupied down = NewLocation $ Sand down
    | checkOccupied downL = NewLocation $ Sand downL
    | checkOccupied downR = NewLocation $ Sand downR
    | maximum (y:map (\(Location _ rpy) -> rpy) (rockPathsToLocations rp)) == y = Abyss
    | otherwise = Settled s
    where
        checkOccupied = isOccupied usedLocs
        down = Location x (y + 1)
        downL = Location (x - 1) (y + 1)
        downR = Location (x + 1) (y + 1)
        usedLocs = getUsedLocations sands rp

getUsedLocations :: [Sand] -> [RockPath] -> [Location]
getUsedLocations s rp = sandLocs++rockLocs
    where
        sandLocs = map (\(Sand l) -> l) s
        rockLocs = rockPathsToLocations rp

rockPathsToLocations :: [RockPath] -> [Location]
rockPathsToLocations = concatMap (\(RockPath l) -> l)

isOccupied :: [Location] -> Location -> Bool
isOccupied usedLocs (Location x y) =
    let sameX = filter (\(Location ox _) -> x == ox) usedLocs
        otherYs = map (\(Location _ oy) -> oy) sameX
        in y `elem` otherYs

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
