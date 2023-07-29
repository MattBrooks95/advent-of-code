module Day20 (
    run
    , parseInput
    ) where

import System.Exit (
    die
    )

import Parsing (
    integer
    )

import Text.Parsec as P

run :: String -> IO ()
run input = do
    print "day20"
    case P.runParser parseInput () "" input of
        Left e -> do
            print $ "parsing error:" ++ show e
            die "bad input"
        Right numbers -> do
            print $ "parsed numbers:" ++ show numbers

parseInput :: P.Parsec String () [Int]
parseInput = P.many (integer <* P.endOfLine) <* P.eof
