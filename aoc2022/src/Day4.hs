module Day4 where

import Text.Read (
    readMaybe
    )

import qualified Data.Text as T (
    splitOn
    , pack
    , unpack
    , Text(..)
    )

data RangePair = RangePair (Int, Int) (Int, Int) deriving (Show)

lineToRangePair :: String -> Maybe RangePair
lineToRangePair line = do
    (left, right) <- case splitLine of
        [x1, x2] -> Just (x1, x2)
        _ -> Nothing
    leftRange <- charsToRange left
    rightRange <- charsToRange right
    return $ RangePair leftRange rightRange
    where
        splitLine = (T.splitOn (T.pack ",") . T.pack) line

charsToRange :: T.Text -> Maybe (Int, Int)
charsToRange input = do
    (left, right) <- case T.splitOn (T.pack "-") input of
        [l, r] -> Just (l, r)
        _ -> Nothing
    leftAsInt <- readText left :: Maybe Int
    rightAsInt <- readText right :: Maybe Int
    return (leftAsInt, rightAsInt)
    where
        readText = readMaybe . T.unpack

rangeOverlaps :: RangePair -> Bool
rangeOverlaps (RangePair (sa, ea) (sb, eb)) = or [
        sa >= sb && sa <= eb
        , ea >= sb && ea <= eb
        , sb >= sa && sb <= ea
        , eb >= sa && eb <= ea
    ]

aContainsB :: RangePair -> Bool
aContainsB (RangePair (sa, ea) (sb, eb)) = and [
        sa >= sb
        , sa <= eb
        , ea <= eb
        , ea >= sb
    ]

bContainsA :: RangePair -> Bool
bContainsA (RangePair left right) = aContainsB (RangePair right left)
