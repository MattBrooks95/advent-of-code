module Day18 where

import Parsing (
    integer
    , comma
    )

import qualified Data.Set as S

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

-- part1: 4628
run :: String -> IO ()
run input = do
    print input
    case runParser parseInput () "" input of
        Left e -> print e
        Right parseResult -> do
            print parseResult
            let cubes = S.fromList parseResult
                answer = getSurfaceArea cubes
            print $ "surface area of " ++ show (length cubes) ++ " is " ++ show answer


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
