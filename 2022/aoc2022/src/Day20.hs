module Day20 (
    run
    , parseInput
    , Item(..)
    , Increment(..)
    , StartIndex(..)
    , CurrentIndex(..)
    , mkItem
    , wrapItems
    , nextIndex
    ) where

import System.Exit (
    die
    )

import Text.Parsec as P

import qualified Data.Sequence as DS
import qualified Data.List as L
import qualified Data.Foldable as F
import Data.Maybe


import Parsing (
    integer
    )


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
            print $ "is list unique?:" ++ show (length numbers == length (L.nub numbers))
            let items = wrapItems numbers
            let asSeq = itemListToSeq items
            -- print asSeq
            let (mixed, _) = mix (asSeq, items)
            print (map (\(Item (Increment num) _ _) -> num) (F.toList mixed))

type MixableList = DS.Seq Item

itemListToSeq :: [Item] -> MixableList
itemListToSeq =  DS.fromList

mix :: (MixableList, [Item]) -> (MixableList, [Item])
mix (items, []) = (items, [])
mix (items, x:xs) = mix (DS.insertAt itemNextIndex x (DS.deleteAt doItemStartIndex items), xs)
    where
        itemNextIndex = nextIndex (DS.length items) x doItemStartIndex
        doItemStartIndex = fromJust $ DS.elemIndexL x items

nextIndex :: Int -> Item -> Int -> Int
nextIndex numItems (Item (Increment move) _ _) currIdx
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
