{-# LANGUAGE OverloadedStrings #-}
module Day20_2 (
    run
    ) where

import System.Exit (die)

import qualified Data.ByteString as BSS (
    readFile
    )
import qualified Data.Word8 as W 
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Data.Either (isLeft)
import Control.Monad (
    when
    )

run :: String -> IO ()
run inputFilePath = do
    fileContents <- BSS.readFile inputFilePath
    let parsedResult = AP.parseOnly parseInput (BSL.fromStrict fileContents)
    when (isLeft parsedResult) (die "failed to parse")
    print $ "day20 attempt 2:" <> " using file at path:" <> inputFilePath
    print parsedResult

parseInput :: AP.Parser [Int]
parseInput = AP.sepBy parseInputNumber APC.endOfLine

parseInputNumber :: AP.Parser Int
parseInputNumber = APC.signed APC.decimal
