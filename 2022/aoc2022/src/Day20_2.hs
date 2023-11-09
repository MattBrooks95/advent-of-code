{-# LANGUAGE OverloadedStrings #-}
module Day20_2 (
    run
    , MixItem(..)
    , MixItemValue(..)
    , MixItemIndex(..)
    , mix
    , mix'
    ) where

import System.Exit (die)
import Prelude hiding (log)

import qualified Data.ByteString as BSS (
    readFile
    )

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.Attoparsec.ByteString.Lazy as AP

import qualified Data.Sequence as S
import Data.Foldable (toList)

import Debug.Trace
import Control.Monad.Writer.Lazy (Writer)
import Control.Monad.Writer (runWriter, tell)
import Data.Maybe (fromJust)

-- so that I can 'disable' the tracing just by changing this definition
myDebug :: String -> a -> a
--myDebug msg val = trace ("\n" ++ msg) val
myDebug _ val = val

newtype MixItemValue = MixItemValue Int deriving (Eq)
instance Show MixItemValue where
    show (MixItemValue v) = "v:" <> show v
newtype MixItemIndex = MixItemIndex Int deriving (Eq)
instance Show MixItemIndex where
    show (MixItemIndex v) = "i:" <> show v
newtype MixItem = MixItem (MixItemValue, MixItemIndex) deriving (Eq)
instance Show MixItem where
    show (MixItem (v, i)) = "(" <> show v <> ", " <> show i <> ")"

getMixVal :: MixItem -> Int
getMixVal (MixItem (MixItemValue v, _)) = v

getMixIdx :: MixItem -> Int
getMixIdx (MixItem (_, MixItemIndex idx)) = idx

type MixItemsList = S.Seq MixItem

run :: String -> IO ()
run inputFilePath = do
    print $ "day20 attempt 2:" <> " using file at path:" <> inputFilePath
    fileContents <- BSS.readFile inputFilePath
    parsedResult <-
        case AP.parseOnly parseInput (BSL.fromStrict fileContents) of
            Left err -> die ("failed to parse" <> err)
            Right pr -> pure pr
    --print parsedResult
    let values = map MixItemValue parsedResult
        indices = map MixItemIndex [0..]
        --withIndices = map MixItem (zip values indices)
        --use zipWith
        withIndices = zipWith (curry MixItem) values indices
        itemsSequence = S.fromList withIndices :: MixItemsList
        (mixed, log) = mix itemsSequence
    coords <- case mixed of
        Left err -> die $ "mixing failed" <> err
        Right safelyMixed -> do
            print safelyMixed
            let zeroIdx = S.findIndexL (\(MixItem (MixItemValue v, _)) -> v == 0) safelyMixed
            print $ "zero index:" <> show zeroIdx
            pure $ getCoordinates (fromJust zeroIdx) safelyMixed
    case coords of
        Nothing -> die "failed to find coordinates in mixed list"
        Just c@(c1@(c1val, c1idx), c2@(c2val, c2idx), c3@(c3val, c3idx)) -> do
            print $ "coords:" <> show c
            print $ "total score:" <> show (sumThreeple (c1val, c2val, c3val))
    --print itemsSequence
    --printed out the values with their indices to to check that I'm not high
    --mapM_ print withIndices
    mapM_ putStrLn log

sumThreeple :: (Int, Int, Int) -> Int
sumThreeple (x, y, z) = x + y + z

-- the value of the item, the idx it was found at
type ProblemCoordinates = (Int, Int)
getCoordinates :: Int -> MixItemsList -> Maybe (ProblemCoordinates, ProblemCoordinates, ProblemCoordinates)
getCoordinates zeroIndex mixedItems = do
    firstCoord <- getMixVal <$> mixedItems S.!? idx1
    secondCoord <- getMixVal <$> mixedItems S.!? idx2
    thirdCoord <- getMixVal <$> mixedItems S.!? idx3
    pure ((firstCoord, idx1), (secondCoord, idx2), (thirdCoord, idx3))
        where
        idx1 = idxFrom0 1000
        idx2 = idxFrom0 2000
        idx3 = idxFrom0 3000
        len = length mixedItems
        idxFrom0 coordOffset = (coordOffset + zeroIndex) `mod` len

mix :: MixItemsList -> (Either String MixItemsList, [String])
mix input = runWriter $ mix' input (toList input)

-- | item list to be mixed -> list of items not yet processed -> items in mixed order
mix' :: MixItemsList -> [MixItem] -> Writer [String] (Either String MixItemsList)
mix' itemsList (x@(MixItem (MixItemValue val, MixItemIndex itemOrigIndex)):xs)
    -- 0 or result of mod w/ length of sequence == 0 then nothing to do because index doesn't change
    | val `mod` length itemsList == 0 = tell ["offset:" <> show val <> " is multiple of list length, skipping"] >> mix' itemsList xs
    | otherwise = case findItemIndex itemsList x of
        Nothing -> pure $ Left $ "failed to find index of item:" <> show x <> "in " <> show itemsList
        Just itemStartIdx ->
            let targetIndex = (itemStartIdx + val) `mod` length itemsList
                -- this arbitrary +1 is probably the issue, I think the condition here is:
                -- was the offset negative (determines sliding direction on insert)
                -- what to do at the left or right edge?
                useTargetIndex = if isNeg then targetIndex else targetIndex + 1
                leftToLeftEdge = isNeg && targetIndex == 0
                rightToRightEdge = (not isNeg) && targetIndex == length itemsList - 1
                (leftItems, rightItems) =
                    S.splitAt (myDebug ("itemStart:" <> show itemStartIdx <> " targIndex:" <> show useTargetIndex) useTargetIndex) itemsList
            in
                tell ["itemOrigIdx:" <> show itemOrigIndex <> " itemVal:" <> show val <> " currentIndex:" <> show itemStartIdx <> " targetIndex:" <> show targetIndex] >>
                --tell ["\nleft:" <> show leftItems <> " right:" <> show rightItems] >>
                    let newItemsList =
                            if itemStartIdx < targetIndex
                            then S.deleteAt itemStartIdx leftItems S.>< (x S.<| rightItems)
                            else (leftItems S.|> x) S.>< S.deleteAt (itemStartIdx - length leftItems) rightItems
                    in -- mix' newItemsList xs
                    -- if they end up on the edge of the array, wrap them around because the rules
                    -- of this problem are weird
                    --I didn't know you could use guards like this, in a let binding no less
                    let nextIterationSequence
                            | leftToLeftEdge = S.deleteAt 0 (newItemsList S.|> x)
                            | rightToRightEdge = S.deleteAt (length newItemsList) (x S.<| newItemsList)
                            | otherwise = newItemsList
                    in mix' nextIterationSequence xs
            where
                isNeg = val < 0
mix' itemsList [] = pure $ Right itemsList

findItemIndex :: MixItemsList -> MixItem -> Maybe Int
findItemIndex = flip S.elemIndexL

parseInput :: AP.Parser [Int]
parseInput = AP.sepBy parseInputNumber APC.endOfLine

parseInputNumber :: AP.Parser Int
parseInputNumber = APC.signed APC.decimal
