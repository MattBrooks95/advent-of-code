module Day18 where

import Parsing (
    integer
    , comma
    )

import Text.Parsec

type Cube = (Int, Int, Int)

run :: String -> IO ()
run input = do
    print input
    case runParser parseInput () "" input of
        Left e -> print e
        Right parseResult -> do
            print parseResult


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
