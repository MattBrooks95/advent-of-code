{-# LANGUAGE OverloadedStrings #-}
module Day20_2 (
    run
    ) where

import System.Exit (die)

import qualified Data.ByteString as BSS (
    readFile
    )

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Sequence as S

newtype MixItemValue = MixItemValue Int
instance Show MixItemValue where
    show (MixItemValue v) = "v:" <> show v
newtype MixItemIndex = MixItemIndex Int
instance Show MixItemIndex where
    show (MixItemIndex v) = "i:" <> show v
newtype MixItem = MixItem (MixItemValue, MixItemIndex)
instance Show MixItem where
    show (MixItem (v, i)) = "(" <> show v <> ", " <> show i <> ")"

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
        itemsSequence = S.fromList withIndices
    print itemsSequence

parseInput :: AP.Parser [Int]
parseInput = AP.sepBy parseInputNumber APC.endOfLine

parseInputNumber :: AP.Parser Int
parseInputNumber = APC.signed APC.decimal
