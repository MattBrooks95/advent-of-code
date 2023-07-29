module Day20 (
    run
    , parseInput
    , Item(..)
    , Increment(..)
    , StartIndex(..)
    , CurrentIndex(..)
    , mkItem
    , wrapItems
    ) where

import System.Exit (
    die
    )

import Parsing (
    integer
    )

import Text.Parsec as P

newtype Increment = Increment Int deriving (Eq, Show)
newtype StartIndex = StartIndex Int deriving (Eq, Show)
newtype CurrentIndex = CurrentIndex Int deriving (Eq, Show)

data Item = Item Increment StartIndex CurrentIndex
    deriving (Eq, Show)

mkItem :: Increment -> StartIndex -> Item
mkItem inc sIdx@(StartIndex idx) = Item inc sIdx (CurrentIndex idx)

wrapItems :: [Int] -> [Item]
wrapItems numbers = [mkItem (Increment num) (StartIndex idx) | (num, idx) <- zip numbers [0..]]

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
