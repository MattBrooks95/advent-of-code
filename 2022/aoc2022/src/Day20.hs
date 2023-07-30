module Day20 (
    run
    , parseInput
    , Item(..)
    --, Increment(..)
    --, StartIndex(..)
    --, CurrentIndex(..)
    , mkItem
    , wrapItems
    , nextIndex
    , mix
    , itemListToSeq
    , MixableList
    ) where

import System.Exit (
    die
    )

import Debug.Trace

import Text.Parsec as P

import qualified Data.Sequence as DS
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Maybe


import Parsing (
    integer
    )


--newtype Increment = Increment Int deriving (Eq, Show)
--newtype StartIndex = StartIndex Int deriving (Eq, Show)
--newtype CurrentIndex = CurrentIndex Int deriving (Eq, Show)

--data Item = Item Increment StartIndex CurrentIndex
--    deriving (Eq, Show)

newtype Item = Item Int
    deriving (Eq, Show)

mkItem :: Int -> Item
mkItem = Item

wrapItems :: [Int] -> [Item]
--wrapItems numbers = [mkItem (Increment num) (StartIndex idx) | (num, idx) <- zip numbers [0..]]
wrapItems = map Item

run :: String -> IO ()
run input = do
    print "day20"
    case P.runParser parseInput () "" input of
        Left e -> do
            print $ "parsing error:" ++ show e
            die "bad input"
        Right numbers -> do
            print $ "parsed numbers:" ++ show numbers
            print $ "is list unique?:" ++ show (length numbers == length (L.nub numbers))
            let items = wrapItems numbers
            let asSeq = itemListToSeq items
            -- print asSeq
            let (mixed, _) = mix (asSeq, items)
            print $ showItems mixed

type MixableList = DS.Seq Item

itemListToSeq :: [Item] -> MixableList
itemListToSeq =  DS.fromList

showItems :: DS.Seq Item -> String
showItems itemSeq = show $ map (\(Item num) -> num) (F.toList itemSeq)

mix :: (MixableList, [Item]) -> (MixableList, [Item])
mix (items, []) = (items, [])
mix (items, x:xs) =
    let nextIter@(nextItems, _) = (DS.insertAt itemNextIndex x (DS.deleteAt doItemStartIndex items), xs)
    in trace (showItems nextItems) (mix nextIter)
    where
        itemNextIndex = nextIndex (DS.length items) x doItemStartIndex
        doItemStartIndex = fromJust $ DS.elemIndexL x items

nextIndex :: Int -> Item -> Int -> Int
nextIndex numItems (Item move) currIdx
    | move == 0 = currIdx
    | move > 0 =
        ((currIdx + (move `mod` numItems)) `mod` numItems) + (if didWrap then 1 else 0)
    | move < 0 =
        let moveAmount = (abs move `mod` numItems)
        in ((currIdx - moveAmount) `mod` numItems) + (if didWrap then (-1) else 0)
    where
        -- if it wraps around the list to the right or left, it's position will need
        -- to be adjusted such that it ends up to the right of the element occupying
        -- that space in the '+' case, and needs moved one to the left in the
        -- '-' case
        didWrap = let nextIdx = currIdx + move in nextIdx < 0 || nextIdx >= numItems

parseInput :: P.Parsec String () [Int]
parseInput = P.many (integer <* P.endOfLine) <* P.eof
