module Day21 (
    run
    ) where

import System.FilePath (
    FilePath
    )

import Data.ByteString as BSS (
    readFile
    )
import qualified Data.Attoparsec.ByteString.Lazy as AP

run :: FilePath -> IO ()
run fp = do
    fileContents <- BSS.readFile fp
    print "Day21"

parse :: AP.Parser
parse = undefined
