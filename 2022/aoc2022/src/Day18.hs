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

-- get the indices of neighboring squires, with locations that are out of the bounds of
-- the vector being filtered out
getIndicesOfNeighborsGuard :: (Int, Int, Int) -> Cube -> [Cube]
getIndicesOfNeighborsGuard (maxX, maxY, maxZ) cb = let possibleNhbrs = getIndicesOfNeighbors cb in
    filter (\(x, y, z) -> and
        [x <= maxX
        , x > 0
        , y <= maxY
        , y > 0
        , z <= maxZ
        , z > 0
        ])
        possibleNhbrs

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

type ColoredGraphItem = (Cube, ColoredLoc)
type ColoredGraph = V.Vector ColoredGraphItem

isUnchecked :: ColoredLoc -> Bool
isUnchecked IsUnchecked = True
isUnchecked _ = False

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
    print $ "volume of 3d space" ++ show (maxX * maxY * maxZ)
    print $ "emptyGraph:" ++ show emptyGraph
    print $ "emptyGraph length:" ++ show (length emptyGraph)
    print $ "initialGraph:" ++ show initialGraph
    print $ "initialGraph length:" ++ show (length initialGraph)
    print $ "num cubes original:" ++ show (length cubes)
    print $ "graph length:" ++ show (length initialGraph)
    print $ "num cubes graph:" ++ show (length (V.filter (\(_, color) -> isCube color) initialGraph))
    let checkCubesInGraph = map (\cube -> let idx = getIndexForCube cube in initialGraph V.!? idx) cubes
    print checkCubesInGraph
    -- check that 1,1,1 is NOT a cube
    print $ show (initialGraph V.!? getIndexForCube (1, 1, 1))
    -- check the max element is in bounds
    print $ show (initialGraph V.!? getIndexForCube (vx, vy, vz))
    return (-1 :: Int)
    where
        initialGraph =  emptyGraph V.// indexedColoredLocsForCubes cubes getIndexForCube
        --emptyGraph = V.replicate (getIndexForCube boundMax + 1) IsUnchecked :: V.Vector ColoredLoc
        emptyGraph = V.fromList uncheckedGraphItems
        uncheckedGraphItems = getUncheckedGraphItems vx vy vz :: [ColoredGraphItem]
        vecDims@(vx, vy, vz) = getVectorDims cubeBounds
        getIndexForCube = getIndexForLocation vx vy vz
        isCubeOpenToAir = isOpenToAir vx vy vz
        cubeBounds@(boundMin, boundMax@(maxX, maxY, maxZ)) = getBoundingBox cubes

getUncheckedGraphItems :: Int -> Int -> Int -> [ColoredGraphItem]
getUncheckedGraphItems vx vy vz = concat [concat [[((x, y, z), IsUnchecked) | z <- [0..vz] ] | y <- [0..vy] ] | x <- [0..vx]]

colorLocations :: V.Vector ColoredLoc -> (Cube -> Bool) -> S.Set Cube -> V.Vector ColoredLoc
colorLocations locs openToAir cubes = undefined
    where
        go :: V.Vector ColoredLoc -> [Cube] -> V.Vector ColoredLoc
        go currLocs [] = undefined
        --go currLocs [] = if length (V.filter (isUnchecked && isOpenToAir) currLocs) then undefined else undefined
        go currLocs (cb:cbs) = undefined

-- checks if the coordinates of a cube are on the outside layers of the grid area
-- this is necessary because we shouldn't be able to start the breadth-first-search from
-- the unchecked air pockets, as there is no way for the steam/water to get there
-- the entry points for the bfs must be on the edges of the coordinate space
isOpenToAir :: Int -> Int -> Int -> Cube -> Bool
isOpenToAir maxX maxY maxZ (x, y, z) = and [
    x == maxX || x == 1
    , y == maxY || y == 1
    , z == maxZ || z == 1
    ]

indexedColoredLocsForCubes :: [Cube] -> (Cube -> Int) -> [(Int, ColoredGraphItem)]
indexedColoredLocsForCubes cubes getIndexForCube = [ (getIndexForCube c, (c,  IsCube)) | c <- cubes]

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
