module Day18 where

import Debug.Trace

import qualified Data.List as L

import Parsing (
    integer
    , comma
    )

import qualified Data.Set as S

import Data.Maybe (
    catMaybes
    )

import qualified Data.Vector as V

import Text.Parsec

type Cube = (Int, Int, Int)

-- function name is bad, this returns the coordinate pairs for the neighbors
-- not their index into the 3d array for part2
getIndicesOfNeighbors :: Cube -> [Cube]
getIndicesOfNeighbors (x, y, z) = [
    (x + 1, y, z) -- right
    , (x, y + 1, z) -- above
    , (x, y, z + 1) -- behind
    , (x - 1, y, z) -- left
    , (x, y - 1, z) -- below
    , (x, y, z - 1)
    ]

isInBounds :: (Int, Int, Int) -> Cube -> Bool
isInBounds (maxX, maxY, maxZ) (x, y, z) = and
        [x <= maxX
        , x >= 0
        , y <= maxY
        , y >= 0
        , z <= maxZ
        , z >= 0
        ]

-- get the indices of neighboring squares, with locations that are out of the bounds of
-- the vector being filtered out
getIndicesOfNeighborsGuard :: (Cube -> Bool) -> Cube -> [Cube]
getIndicesOfNeighborsGuard inBoundsCheck cb = let possibleNhbrs = getIndicesOfNeighbors cb in
    filter inBoundsCheck possibleNhbrs

getCoveredSidesForCube :: Cube -> S.Set Cube -> Int
getCoveredSidesForCube cb cubes = let nhbrs = getIndicesOfNeighbors cb in
    length (filter (flip S.member cubes) nhbrs)

getCoveredSidesForCubePart2 :: (Cube -> Int) -> Cube -> ColoredGraph -> Int
getCoveredSidesForCubePart2 getIdx cb graph =
    let nhbrs = getIndicesOfNeighbors cb
        cubes = getCubes graph getIdx nhbrs
    in
    --length (filter (not . indexIsAir graph) (map getIdx nhbrs)) -- 2544
    length (filter (\(_, color) -> not (isAir color)) cubes) -- 2545?

getCubes :: ColoredGraph -> (Cube -> Int) -> [Cube] -> [ColoredGraphItem]
getCubes graph getIdx cbs =
    let ids = map getIdx cbs
        cubes = map (graph V.!?) ids
    in
        catMaybes cubes

--indexIsAir :: ColoredGraph -> Int -> Bool
--indexIsAir graph idx = case graph V.!? idx of
--    Just (_, IsAir) -> True
--    _ -> False

getSurfaceArea :: S.Set Cube -> Int
getSurfaceArea cubes = totalPossibleSurfaceArea - numHiddenSides
    where
        totalPossibleSurfaceArea = length cubes * 6
        numHiddenSides = foldr (\cb acc -> acc + getCoveredSidesForCube cb cubes) 0 cubes

getSurfaceAreaPart2 :: (Cube -> Int) -> [Cube] -> ColoredGraph -> Int
getSurfaceAreaPart2 getIdx cubes graph = totalPossibleSurfaceArea - numHiddenSides
    where
        totalPossibleSurfaceArea = length cubes * 6
        numHiddenSides = foldr (\cb acc -> acc + getCoveredSidesForCubePart2 getIdx cb graph) 0 cubes
        --numHiddenSides = length (V.filter (\(_, col) -> isUnchecked col) graph) * 6 --logic is wrong and of course guess 7692 was also wrong


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
-- part2: 2544 is too low, but it was correct on the short input
-- 7692 is too high, this is the number: numCubes * 6 - 6 * (number of unchecked locations)
-- which makes sense, those unchecked locations are bordering eachother, so not all of them are going to
-- take away 6 sides from the surface area of the cubes
run :: String -> IO ()
run input = do
    --print input
    case runParser parseInput () "" input of
        Left e -> print e
        Right parseResult -> do
            --print parseResult
            let surfaceArea = part1 parseResult
            print $ "surface area of " ++ show (length parseResult) ++ " cubes is " ++ show surfaceArea
            exteriorSurfaceArea <- part2 parseResult
            print $ "external surface area is: " ++ show exteriorSurfaceArea

data ColoredLoc = IsCube | IsAir | IsUnchecked | IsChecking deriving Show
isCube :: ColoredLoc -> Bool
isCube IsCube = True
isCube _ = False

isAir :: ColoredLoc -> Bool
isAir IsAir = True
isAir _ = False

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
    --print $ "min:" ++ show boundMin
    print $ "max:" ++ show boundMax
    print $ "space dim:" ++ show spaceDimension
    print $ "dimensions:" ++ show vecDims
    print $ "arrayLength" ++ show arrayLength ++ " maximum index:" ++ show maximumIndex
    print $ "empty graph length:" ++ show (length emptyGraph)
    print $ "empty graph element at:" ++ show maximumIndex ++ " " ++ show (emptyGraph V.!? (maximumIndex))
    --print $ "unchecked graph items:" ++ show uncheckedGraphItems
    --print $ "init graph length:" ++ show (length unInitGraph)
    --print $ "emptyGraph:" ++ show emptyGraph
    --print $ "emptyGraph length:" ++ show (length emptyGraph)
    --print $ "initialGraph:" ++ show initialGraph
    --print $ "initialGraph length:" ++ show (length initialGraph)
    --print $ "num cubes original:" ++ show (length cubes)
    --print $ "graph length:" ++ show (length initialGraph)
    --print $ "num cubes graph:" ++ show (length (V.filter (\(_, color) -> isCube color) initialGraph))
    --let checkCubesInGraph = map (\cube -> let idx = getIndexForCube cube in initialGraph V.!? idx) cubes
    --print checkCubesInGraph
    --these checks are meaningful for the short input, not the real input
    ---- check that 1,1,1 is NOT a cube
    testGetIndex getIndexForCube (1, 1, 1) initialGraph
    ---- check the max element is in bounds
    testGetIndex getIndexForCube (vx, vy, vz) initialGraph
    testGetIndex getIndexForCube (1, 1, 5) initialGraph
    testGetIndex getIndexForCube (1, 2, 2) initialGraph
    testGetIndex getIndexForCube (2, 2, 3) initialGraph

    let coloredGraph = colorLocations initialGraph isCubeOpenToAir cubeIsInBounds (getIndicesOfNeighborsGuard cubeIsInBounds) getIndexForCube
    --print coloredGraph
    mapM_ putStrLn [
        "with " ++ show (length cubes) ++ " cubes," ++ " and a graph of size:" ++ show arrayLength
        , "the total possible surface area is:" ++ show (length cubes * 6)
        , show (length (V.filter(\(_, color) -> isAir color) coloredGraph)) ++ " sections where open to the atmosphere"
        , "and " ++ show (length (V.filter(\(_, color) -> isUnchecked color) coloredGraph)) ++ " were unchecked"
        ]
    let sa = getSurfaceAreaPart2 getIndexForCube cubes coloredGraph
    return sa
    where
        --initialGraph =  emptyGraph V.// trace ("cube color loc init:"  ++ show cubeColorLocInit) cubeColorLocInit
        initialGraph =  emptyGraph V.// cubeColorLocInit
        cubeColorLocInit = indexedColoredLocsForCubes cubes getIndexForCube
        --emptyGraph = V.replicate (getIndexForCube boundMax + 1) IsUnchecked :: V.Vector ColoredLoc
        --emptyGraph = unInitGraph V.// uncheckedGraphItems
        --unInitGraph = V.replicate (spaceDimension ^ (3 :: Int)) unInitializedLocation
        --emptyGraph = V.replicate arrayLength unInitializedLocation -- this worked for the short input of part2
        ---- but the colors still being marked -1,-1,-1 made the 'find a new start point' logic incorrect
        emptyGraph = V.replicate arrayLength unInitializedLocation V.// getEmptyGraph spaceDimension getIndexForCube
        uncheckedGraphItems = getUncheckedGraphItems spaceDimension getIndexForCube :: [(Int, ColoredGraphItem)]
        vecDims@(vx, vy, vz) = getVectorDims cubeBounds
        getIndexForCube = getIndexForLocation spaceDimension
        isCubeOpenToAir = isOpenToAir spaceDimension spaceDimension spaceDimension
        cubeIsInBounds c = let idx = getIndexForCube c in idx <= maximumIndex && idx >= 0 && dimIsInBounds spaceDimension c
        arrayLength = maximumIndex + 1
        spaceDimension = maximum [maxX, maxY, maxZ] + 1 :: Int
        cubeBounds@(_, boundMax@(maxX, maxY, maxZ)) = getBoundingBox cubes
        maximumIndex = getIndexForCube (spaceDimension, spaceDimension, spaceDimension)
        unInitializedLocation = ((-1, -1, -1), IsUnchecked)

-- TODO de-duplicate this with the IO version
runPart2 :: [Cube] -> (Int, ColoredGraph)
runPart2 cubes = 
    let coloredGraph = colorLocations initialGraph isCubeOpenToAir cubeIsInBounds (getIndicesOfNeighborsGuard cubeIsInBounds) getIndexForCube
        sa = getSurfaceAreaPart2 getIndexForCube cubes coloredGraph
    in (sa, coloredGraph)
    where
        initialGraph =  emptyGraph V.// cubeColorLocInit
        emptyGraph = V.replicate arrayLength unInitializedLocation V.// getEmptyGraph spaceDimension getIndexForCube
        arrayLength = maximumIndex + 1
        unInitializedLocation = ((-1, -1, -1), IsUnchecked)
        cubeColorLocInit = indexedColoredLocsForCubes cubes getIndexForCube
        spaceDimension = maximum [maxX, maxY, maxZ] + 1 :: Int
        getIndexForCube = getIndexForLocation spaceDimension
        cubeBounds@(_, boundMax@(maxX, maxY, maxZ)) = getBoundingBox cubes
        maximumIndex = getIndexForCube (spaceDimension, spaceDimension, spaceDimension)
        isCubeOpenToAir = isOpenToAir spaceDimension spaceDimension spaceDimension
        cubeIsInBounds c = let idx = getIndexForCube c in idx <= maximumIndex && idx >= 0 && dimIsInBounds spaceDimension c

genCube :: Int -> [Cube]
genCube dim = concat [ concat [ [(x, y, z) | z <- dimRange] | y <- dimRange ] | x <- dimRange]
    where
        dimRange = [1..dim]

getEmptyGraph :: Int -> (Cube -> Int) -> [(Int, ColoredGraphItem)]
getEmptyGraph spaceDimension idxForCube =
    concat [concat [[ let cb = (x, y, z) in (idxForCube cb, (cb, IsUnchecked)) | z <- dimRange] | y <- dimRange] | x <- dimRange]
        where
            dimRange = [0..spaceDimension]

dimIsInBounds :: Int -> Cube -> Bool
dimIsInBounds maxDim (cx, cy, cz) =
        inBounds cx
        && inBounds cy
        && inBounds cz
        where
            inBounds x = x >= 0 && x <= maxDim
    --length (filter (\x -> x >= 0 && x <= maxDim) [cy, cx, cz]) == 3

testGetIndex :: (Cube -> Int) -> Cube -> ColoredGraph -> IO ()
testGetIndex getIndexForCube cb graph =
    print $ show cb ++ indexMsg ++ " " ++ show (graph V.!? idx)
    where
        indexMsg = " generates index:" ++ show idx
        idx = getIndexForCube cb

getUncheckedGraphItems :: Int -> (Cube -> Int) -> [(Int, ColoredGraphItem)]
getUncheckedGraphItems spaceDimension getCubeIndex = concat [concat [[(getCubeIndex (x, y, z), ((x, y, z), IsUnchecked)) | z <- [0..spaceDimension] ] | y <- [0..spaceDimension] ] | x <- [0..spaceDimension]]

colorLocations :: ColoredGraph -> (Cube -> Bool) -> (Cube -> Bool) -> (Cube -> [Cube]) -> (Cube -> Int) ->  ColoredGraph
--colorLocations locs openToAir cubeIsInBounds getNhbrs getIndexForCube = go locs [(0, 0, 0)]
colorLocations locs openToAir cubeIsInBounds getNhbrs getIndexForCube = go locs []
    where
        go :: ColoredGraph -> [Cube] -> ColoredGraph
        go currLocs [] = --currLocs
            ---- if there are no more locations queued, see if we can find a new start point
            ---- if we can't, the process is over and just return the colored graph
            --case V.find (\(cb, color) -> isUnchecked (trace (" looking for start:" ++ show color ++ " cube:" ++ show cb) color) && openToAir cb && cubeIsInBounds cb) currLocs of
            case V.find (\item@(cb, color) -> isStartingPoint openToAir cubeIsInBounds item) currLocs of
                Nothing -> currLocs
                Just (newStart, clr) ->
                    let newGraph = (currLocs V.// [(getIndexForCube (trace ("found:" ++ show newStart ++ " of color:" ++ show clr) newStart), (newStart, IsChecking))]) in
                    --go (trace ("new graph:" ++ show newGraph) newGraph) [newStart]
                    go newGraph [newStart]
        go currLocs (cb:cbs) =
            --let nhbrs = getNhbrs (trace ("inspecting:" ++ show cb) cb)
            let nhbrs = getNhbrs cb
                --uncheckedNhbrs = filter (\ncb -> let (_, color) = getGraphNode currLocs getIndexForCube ncb in isUnchecked color) (trace ("nhbrs" ++ show nhbrs) nhbrs)
                uncheckedNhbrs = filter (\ncb -> let (_, color) = getGraphNode currLocs getIndexForCube ncb in isUnchecked color) nhbrs
                --listToUpdateNeighbors = map (\checkCb -> (getIndexForCube checkCb, (checkCb, IsChecking))) (trace ("\tuncheckedn:" ++ show uncheckedNhbrs) uncheckedNhbrs)
                listToUpdateNeighbors = map (\checkCb -> (getIndexForCube checkCb, (checkCb, IsChecking))) uncheckedNhbrs
                --withNewPaintedNodes = currLocs V.// ((getIndexForCube cb, (cb, IsAir)):trace ("\tto update neighbors" ++ show listToUpdateNeighbors) listToUpdateNeighbors)
                withNewPaintedNodes = currLocs V.// ((getIndexForCube cb, (cb, IsAir)):listToUpdateNeighbors)
            in go withNewPaintedNodes (uncheckedNhbrs++cbs)

isStartingPoint :: (Cube -> Bool) -> (Cube -> Bool) -> ColoredGraphItem -> Bool
isStartingPoint isOpenToAirF cubeIsInBounds (cb, color) =
    --trace ("cb:" ++ show cb ++ inBoundsMsg ++ openToAirMsg ++ isUncheckedMsg)
        inBounds && openToAir && isUncheckedRes
    where
        inBounds = cubeIsInBounds cb
        inBoundsMsg = " is in bounds?:" ++ show inBounds
        openToAirMsg = " is open to air?:" ++ show openToAir
        openToAir = isOpenToAirF cb
        isUncheckedRes = isUnchecked color
        isUncheckedMsg = " is unchecked?:" ++ show isUncheckedRes
--case V.find (\(cb, color) -> isUnchecked (trace (" looking for start:" ++ show color ++ " cube:" ++ show cb) color) && openToAir cb && cubeIsInBounds cb) currLocs of

getGraphNode :: ColoredGraph -> (Cube -> Int) -> Cube -> ColoredGraphItem
getGraphNode graph getIndexForCube cb =
    let idx = getIndexForCube cb
        result = graph V.! idx in
    --trace ("found cube:" ++ show cb ++ " which has index:" ++ show idx ++ " res:" ++ show result) result
    result

-- checks if the coordinates of a cube are on the outside layers of the grid area
-- this is necessary because we shouldn't be able to start the breadth-first-search from
-- the unchecked air pockets, as there is no way for the steam/water to get there
-- the entry points for the bfs must be on the edges of the coordinate space
isOpenToAir :: Int -> Int -> Int -> Cube -> Bool
isOpenToAir maxX maxY maxZ (x, y, z) =
    x >= maxX || x == 0
    || y >= maxY || y == 0
    || z >= maxZ || z == 0

indexedColoredLocsForCubes :: [Cube] -> (Cube -> Int) -> [(Int, ColoredGraphItem)]
indexedColoredLocsForCubes cubes getIndexForCube = [ (getIndexForCube c, (c,  IsCube)) | c <- cubes]

-- get an index for a cube that is being held in a 1d Vector
-- this is one indexed!
getIndexForLocation :: Int -> Cube -> Int
getIndexForLocation spaceDimension (x, y, z) = 
    (x * (spaceDimension ^ 2) + y * spaceDimension + z)

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
