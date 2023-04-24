module Day18 where

import Parsing (
    integer
    , comma
    )

import qualified Data.Set as S

import qualified Data.Vector as V

import Text.Parsec

type Cube = (Int, Int, Int)

getIndicesOfNeighbors :: Cube -> [Cube]
getIndicesOfNeighbors (x, y, z) = [
    (x + 1, y, z) -- right
    , (x, y + 1, z) -- above
    , (x, y, z + 1) -- behind
    , (x - 1, y, z) -- left
    , (x, y - 1, z) -- below
    , (x, y, z - 1)
    ]

getCoveredSidesForCube :: Cube -> S.Set Cube -> Int
getCoveredSidesForCube cb cubes = let nhbrs = getIndicesOfNeighbors cb in
    length (filter (flip S.member cubes) nhbrs)

getSurfaceArea :: S.Set Cube -> Int
getSurfaceArea cubes = totalPossibleSurfaceArea - numHiddenSides
    where
        totalPossibleSurfaceArea = length cubes * 6
        numHiddenSides = foldr (\cb acc -> acc + getCoveredSidesForCube cb cubes) 0 cubes

minInt :: Int
minInt = minBound

minCubeLoc :: Cube
minCubeLoc = (minInt, minInt, minInt)

maxInt :: Int
maxInt = maxBound

maxCubeLoc :: Cube
maxCubeLoc = (maxInt, maxInt, maxInt)

getBoundingBox :: [Cube] -> (Cube, Cube)
getBoundingBox cubes = go cubes (maxCubeLoc, minCubeLoc)
    where
    go :: [Cube] -> (Cube, Cube) -> (Cube, Cube)
    go [] acc = acc
    go ((cx, cy, cz):cbs) ((minx, miny, minz), (maxx, maxy, maxz)) = 
        let newMin = (min minx cx, min miny cy, min minz cz)
            newMax = (max maxx cx, max maxy cy, max maxz cz) in
        go cbs (newMin, newMax)

-- part1: 4628
run :: String -> IO ()
run input = do
    print input
    case runParser parseInput () "" input of
        Left e -> print e
        Right parseResult -> do
            print parseResult
            --let surfaceArea = part1 parseResult
            --print $ "surface area of " ++ show (length parseResult) ++ " is " ++ show surfaceArea
            exteriorSurfaceArea <- part2 parseResult
            print $ "external surface area is: " ++ show exteriorSurfaceArea

data ColoredLoc = IsCube | IsAir | IsUnchecked deriving Show
isCube :: ColoredLoc -> Bool
isCube IsCube = True
isCube _ = False

-- idea: start just outside of the minimum or maximum point of the cube
-- make a big vector of spaces that are colored, like in djikstra's algorithm
-- then, from the start point do a breadth first search, coloring in the air spaces
-- as being a special color
-- then, every point in the lava droplet that is accessible from the outside will be known
-- the air pockets inside the lava will not be found by the BFS, so we don't count
-- them as being a neighbor of a cube when we determine the surface area
part2 :: [Cube] -> IO Int
part2 cubes = do
    print $ "min:" ++ show boundMin
    print $ "max:" ++ show boundMax
    print $ "dimensions:" ++ show vecDims
    print $ "emptyGraph:" ++ show emptyGraph
    print $ "emptyGraph length:" ++ show (length emptyGraph)
    print $ "initialGraph:" ++ show initialGraph
    print $ "num cubes original:" ++ show (length cubes)
    print $ "graph length:" ++ show (length initialGraph)
    print $ "num cubes graph:" ++ show (length (V.filter isCube initialGraph))
    let checkCubesInGraph = map (\cube -> let idx = getIndexForCube cube in initialGraph V.!? idx) cubes
    print checkCubesInGraph
    -- check that 1,1,1 is NOT a cube
    print $ show (initialGraph V.!? getIndexForCube (1, 1, 1))
    -- check the max element is in bounds
    print $ show (initialGraph V.!? getIndexForCube (vx, vy, vz))
    return (-1 :: Int)
    where
        initialGraph =  emptyGraph V.// indexedColoredLocsForCubes cubes getIndexForCube
        emptyGraph = V.replicate (getIndexForCube boundMax + 1) IsUnchecked :: V.Vector ColoredLoc
        vecDims@(vx, vy, vz) = getVectorDims cubeBounds
        getIndexForCube = getIndexForLocation vx vy vz
        cubeBounds@(boundMin, boundMax) = getBoundingBox cubes

indexedColoredLocsForCubes :: [Cube] -> (Cube -> Int) -> [(Int, ColoredLoc)]
indexedColoredLocsForCubes cubes getIndexForCube = [ (getIndexForCube c, IsCube) | c <- cubes]

-- get an index for a cube that is being held in a 1d Vector
-- this is one indexed!
getIndexForLocation :: Int -> Int -> Int -> Cube -> Int
getIndexForLocation numRows numCols numDepth (x, y, z) = 
    (x - 1) * (numCols * numDepth) + (y - 1) * numDepth + z

getVectorDims :: (Cube, Cube) -> (Int, Int, Int)
getVectorDims ((minx, miny, minz), (maxx, maxy, maxz)) =
    (maxx - minx, maxy - miny, maxz - minz)

part1 :: [Cube] -> Int
part1 cubeList =
    let cubes = S.fromList cubeList
        answer = getSurfaceArea cubes
    in
    answer

parseLine :: Parsec String () Cube
parseLine = do
    x <- integer
    _ <- comma
    y <- integer
    _ <- comma
    z <- integer
    return (x, y, z)


parseInput :: Parsec String () [Cube]
parseInput = many (parseLine <* endOfLine) <* eof
