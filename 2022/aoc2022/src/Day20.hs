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
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe


import Parsing (
    integer
    )


myTrace :: String -> a -> a
--myTrace = trace
myTrace _ something = something

--newtype Increment = Increment Int deriving (Eq, Show)
--newtype StartIndex = StartIndex Int deriving (Eq, Show)
--newtype CurrentIndex = CurrentIndex Int deriving (Eq, Show)

--data Item = Item Increment StartIndex CurrentIndex
--    deriving (Eq, Show)

newtype Item = Item Int
    deriving (Eq, Show)

itemVal :: Item -> Int
itemVal (Item x) = x

mkItem :: Int -> Item
mkItem = Item

wrapItems :: [Int] -> [Item]
--wrapItems numbers = [mkItem (Increment num) (StartIndex idx) | (num, idx) <- zip numbers [0..]]
wrapItems = map Item

-- part1, 5169 is too low
run :: String -> IO ()
run input = do
    print "day20"
    case P.runParser parseInput () "" input of
        Left e -> do
            print $ "parsing error:" ++ show e
            die "bad input"
        Right numbers -> do
            --print $ "parsed numbers:" ++ show numbers
            print $ "number count:" ++ show (length numbers)
            print $ "unique item count:" ++ show ((length . L.nub) numbers)
            print $ "is list unique?:" ++ show (length numbers == length (L.nub numbers))
            let itemCountList = countOccurences numbers
            print $ "duped items:" ++ show (M.filter (> 1) itemCountList)
            let items = wrapItems numbers
            let asSeq = itemListToSeq items
            -- print asSeq
            let (mixed, _) = mix (asSeq, items)
            --print $ showItems mixed
            let part1AnswerIndices = [1000, 2000, 3000] :: [Int]
                maxIndex = length numbers
                idxOfZero = fromJust $ DS.elemIndexL (Item 0) mixed
                part1AnswerIndicesWrapped = map (\offset -> (offset + idxOfZero) `mod` maxIndex) part1AnswerIndices
                part1AnswerItems = mapMaybe (mixed DS.!?) part1AnswerIndicesWrapped
            print $ "index of 0:" ++ show idxOfZero
            print $ "part 1 answer wrappedIndices:" ++ show part1AnswerIndicesWrapped
            --print $ "part 1 answer items:" ++ show part1AnswerItems
            print $ "part 1 answer:" ++ show (sum (map itemVal part1AnswerItems))

countOccurences :: [Int] -> M.Map Int Int
countOccurences numbers = go numbers M.empty
    where
        go [] acc = acc
        go (x:xs) acc = case M.lookup x acc of
            Just foundItemCount -> go xs (M.update (\_ -> Just (foundItemCount + 1)) x acc)
            Nothing -> go xs (M.insert x 1 acc)

type MixableList = DS.Seq Item

itemListToSeq :: [Item] -> MixableList
itemListToSeq =  DS.fromList

showItems :: DS.Seq Item -> String
showItems itemSeq = show $ map (\(Item num) -> num) (F.toList itemSeq)

mix :: (MixableList, [Item]) -> (MixableList, [Item])
mix (items, []) = (items, [])
mix (items, x:xs) =
    let nextIter@(nextItems, _) = (DS.insertAt itemNextIndex x (DS.deleteAt doItemStartIndex items), xs)
    in myTrace (showItems nextItems) (mix nextIter)
    where
        itemNextIndex = nextIndex (DS.length items) x doItemStartIndex
        doItemStartIndex = fromJust $ DS.elemIndexL x items

nextIndex :: Int -> Item -> Int -> Int
nextIndex numItems (Item move) currIdx
    | move == 0 = currIdx
    | move > 0 =
        ((currIdx + (move `mod` numItems)) `mod` numItems) + (if didWrap then 1 else 0)
    | isMinus =
        let moveAmount = (abs move `mod` numItems)
            finalIndex = ((currIdx - moveAmount) `mod` numItems) + (if didWrap then (-1) else 0)
        in myTrace
            ("moving:" ++ show move ++ " final index:" ++ show finalIndex)
            (if finalIndex == 0 then numItems else finalIndex)
    where
        -- if it wraps around the list to the right or left, it's position will need
        -- to be adjusted such that it ends up to the right of the element occupying
        -- that space in the '+' case, and needs moved one to the left in the
        -- '-' case
        didWrap = let nextIdx = currIdx + move in nextIdx < 0 || nextIdx >= numItems
        isMinus = move < 0

parseInput :: P.Parsec String () [Int]
parseInput = P.many (integer <* P.endOfLine) <* P.eof
