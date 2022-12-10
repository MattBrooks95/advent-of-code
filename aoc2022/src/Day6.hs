module Day6 (
    run
    , startElemsAreUnique
    ) where

import Control.Monad (
    when
    )

import qualified Data.Set as S


import Prelude hiding (lines)
import qualified Prelude as P (
    lines
    , take
    )


run :: [String] -> IO ()
run lines = do
    print "day6"
    when (length lines /= 1) (do
        print "input was not a single line"
        return ())
    let (startSignal, _) = findStartSignal 4 ([], head lines)
    print $ "part1| untilStartSignal (reversed): " ++ startSignal ++ " length: " ++ show (length startSignal)
    let (startMessageSignal, _) = findStartSignal 14 ([], head lines)
    print $ "part2| untilStartSignal (reversed): " ++ startMessageSignal ++ " length: " ++ show (length startMessageSignal)

-- lengthOfStartSequence (alreadyProcessedChars, remaining chars)
findStartSignal :: Int -> (String, String) -> (String, String)
findStartSignal startSeqLength (done, []) = (done, [])
findStartSignal startSeqLength (done, x:xs) =
    if (length done >= startSeqLength) && startElemsAreUnique startSeqLength final then  
        (final, xs)
    else
        findStartSignal startSeqLength (final, xs)
    where
        final = x:done


startElemsAreUnique :: (Ord a) => Int -> [a] -> Bool
startElemsAreUnique checkLength list = length (S.fromList inspectElements) == length inspectElements
    where
        inspectElements = P.take checkLength list
