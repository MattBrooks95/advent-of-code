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

import qualified Data.ByteString as BSS (
    readFile
    )

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.Attoparsec.ByteString.Lazy as AP

import qualified Data.Sequence as S
import Data.Foldable (toList)

import Debug.Trace

-- so that I can 'disable' the tracing just by changing this definition
myDebug :: String -> a -> a
myDebug msg val = trace msg val

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
        mixed = mix itemsSequence
    coords <- case mixed of
        Left err -> die $ "mixing failed" <> err
        Right safelyMixed -> do
            print safelyMixed
            pure $ getCoordinates safelyMixed
    case coords of
        Nothing -> die "failed to find coordinates in mixed list"
        Just c -> do
            print $ "coords:" <> show c
            print $ "total score:" <> show (sumThreeple c)
    --print itemsSequence

sumThreeple :: (Int, Int, Int) -> Int
sumThreeple (x, y, z) = x + y + z

getCoordinates :: MixItemsList -> Maybe (Int, Int, Int)
getCoordinates mixedItems = do
    zeroIndex <- S.findIndexL (\(MixItem (MixItemValue v, _)) -> v == 0) mixedItems
    let getCoordIdx = idxFrom0 zeroIndex
    firstCoord <- getMixVal <$> mixedItems S.!? getCoordIdx 1000
    secondCoord <- getMixVal <$> mixedItems S.!? getCoordIdx 2000
    thirdCoord <- getMixVal <$> mixedItems S.!? getCoordIdx 3000
    pure (firstCoord, secondCoord, thirdCoord)
        where
        len = length mixedItems
        idxFrom0 zeroIndex coordOffset = (coordOffset + zeroIndex) `mod` len

mix :: MixItemsList -> Either String MixItemsList
mix input = mix' input (toList input)

-- | item list to be mixed -> list of items not yet processed -> items in mixed order
mix' :: MixItemsList -> [MixItem] -> Either String MixItemsList
mix' itemsList (x@(MixItem (MixItemValue val, MixItemIndex _)):xs)
    -- 0 or result of mod w/ length of sequence == 0 then nothing to do because index doesn't change
    | myDebug ("val:" <> show val) val `mod` length itemsList == 0 = myDebug "offset multiple of list length, skipping" (mix' itemsList xs)
    | otherwise = case myDebug ("outer case, val:" <> show val) (findItemIndex itemsList x) of
        Nothing -> Left $ "failed to find index of item:" <> show x <> "in " <> show itemsList
        Just itemStartIdx -> myDebug "just case" $
            let targetIndex = itemStartIdx + val `mod` length itemsList
                (leftItems, rightItems) = S.splitAt (if isNeg then targetIndex else targetIndex + 1) itemsList
            in
                myDebug ("left:" <> show leftItems <> " right:" <> show rightItems) $
                    let newItemsList = myDebug ("startIdx:" <> show itemStartIdx <> " offset: " <> show val) $ if itemStartIdx < targetIndex
                        then S.deleteAt itemStartIdx leftItems S.>< (x S.<| rightItems)
                        else (leftItems S.|> x) S.>< S.deleteAt (itemStartIdx - length leftItems) rightItems
                    in myDebug ("recursing, new list:" <> show newItemsList) (mix' newItemsList xs)
            where
                isNeg = val < 0
mix' itemsList [] = Right itemsList

findItemIndex :: MixItemsList -> MixItem -> Maybe Int
findItemIndex = flip S.elemIndexL

parseInput :: AP.Parser [Int]
parseInput = AP.sepBy parseInputNumber APC.endOfLine

parseInputNumber :: AP.Parser Int
parseInputNumber = APC.signed APC.decimal
