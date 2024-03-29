module Day20 (
    run
    , parseInput
    , Item(..)
    --, Increment(..)
    --, StartIndex(..)
    --, CurrentIndex(..)
    , mkItem
    , wrapItems
    --, nextIndex
    , mix
    , itemListToSeq
    , MixableList
    , ItemOrigIndex(..)
    , ItemValue(..)
    , findItemInSeq
    , makeSeq
    ) where

import Prelude hiding (
    Left
    , Right
    )
import qualified Data.Either as E

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

import Control.Monad (
    when
    )

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

data InsertDir = Left | Right
    deriving (Show)

newtype ItemValue = ItemValue Int
    deriving (Eq, Show, Ord)
newtype ItemOrigIndex = ItemOrigIndex Int
    deriving (Eq, Show, Ord)

data Item = Item ItemValue ItemOrigIndex
    deriving (Eq, Ord)

instance Show Item where
    show (Item (ItemValue val) (ItemOrigIndex origIdx)) =
        "(v:" ++ show val ++ ", oIdx:" ++ show origIdx ++ ")"

itemVal :: Item -> Int
itemVal (Item (ItemValue x) _) = x

mkItem :: ItemValue -> ItemOrigIndex -> Item
mkItem = Item

wrapItems :: [Int] -> [Item]
--wrapItems numbers = [mkItem (Increment num) (StartIndex idx) | (num, idx) <- zip numbers [0..]]
wrapItems numbers = map (\(val, origIdx) -> Item (ItemValue val) (ItemOrigIndex origIdx)) (zip numbers [0..])

makeSeq :: [Item] -> DS.Seq Item
makeSeq = DS.fromList

-- part1, 5169 is too low
-- -11323 no good
-- 12962 was no good, after fixing several bugs
run :: String -> IO ()
run input = do
    print "day20"
    case P.runParser parseInput () "" input of
        E.Left e -> do
            print $ "parsing error:" ++ show e
            die "bad input"
        E.Right numbers -> do
            --print $ "numbers:" ++ show numbers
            --print $ "parsed numbers:" ++ show numbers
            print $ "number count:" ++ show (length numbers)
            print $ "unique item count:" ++ show ((length . L.nub) numbers)
            print $ "is list unique?:" ++ show (length numbers == length (L.nub numbers))
            -- let itemCountList = countOccurences numbers
            -- print $ "duped items:" ++ show (M.filter (> 1) itemCountList)
            let items = wrapItems numbers
            print $ "is list w/origIndices unique?:" ++ show (S.size (S.fromList items) == length numbers)
            print $ "items at start:" ++ show items
            let zeroItem = F.find (\(Item (ItemValue val) _) -> val == 0) items
            when (isNothing zeroItem) (die "couldn't find the 0 item")
            let asSeq = itemListToSeq items
            -- print asSeq
            let (mixed, _) = mix (asSeq, items)
            --print $ showItems mixed
            let part1AnswerIndices = (1000, 2000, 3000) :: (Int, Int, Int)
                idxOfZero = fromJust $ DS.elemIndexL (fromJust zeroItem) mixed
                part1AnswerItems = getPart1AnswerItems (1000, 2000, 3000) mixed
            --print $ "as seq" ++ show asSeq
            print $ "index of 0:" ++ show idxOfZero
            --print $ "part 1 answer items:" ++ show part1AnswerItems
            let answerItems = map itemVal part1AnswerItems
            print $ "part 1 answer items:" ++ show answerItems
            print $ "part 1 answer:" ++ show (sum answerItems)
            let startItems = S.fromList (F.toList items)
                endItems = S.fromList (F.toList mixed)
                startEndDiff = S.difference startItems endItems
            -- ensure that we didn't lose data
            print $ "difference length: " ++ show (length startEndDiff) ++ "start list and end list difference:" ++ show startEndDiff
            print $ "num items start:" ++ show (length startItems) ++ " num items end:" ++ show (length endItems)
            let lastItem = fromJust $ DS.lookup (length mixed - 1) mixed
                firstItem = fromJust $ DS.lookup 0 mixed
            print $ "off by one?"
                 -- how does this not change the answer????????
                 -- oh, it's because we start counting from 0, so removing from the left one item and
                 -- adding to the right one item doesn't change anything, I'd have to shift 0 to the left or the right once
                 -- to test this theory
                 ++ show (sum (map itemVal (getPart1AnswerItems part1AnswerIndices ((lastItem DS.<| DS.deleteAt (length mixed - 1) mixed)))))

getPart1AnswerItems :: (Int, Int, Int) -> MixableList -> [Item]
getPart1AnswerItems (idxOne, idxTwo, idxThree) items =
    let maxIndex = length items
        zeroItem = F.find (\(Item (ItemValue val) _) -> val == 0) items
        idxOfZero = fromJust $ DS.elemIndexL (fromJust zeroItem) items
        part1AnswerIndicesWrapped = map (\offset -> (offset + idxOfZero) `mod` maxIndex) [idxOne, idxTwo, idxThree]
    in map (\idx -> fromJust (items DS.!? idx)) part1AnswerIndicesWrapped

findItemInSeq :: ItemValue -> DS.Seq Item -> Maybe Item
findItemInSeq (ItemValue findVal) =
    F.find (\(Item (ItemValue val) _) -> val == findVal)

--countOccurences :: [Int] -> M.Map Int Int
--countOccurences numbers = go numbers M.empty
--    where
--        go [] acc = acc
--        go (x:xs) acc = case M.lookup x acc of
--            Just foundItemCount -> go xs (M.update (\_ -> Just (foundItemCount + 1)) x acc)
--            Nothing -> go xs (M.insert x 1 acc)

type MixableList = DS.Seq Item

itemListToSeq :: [Item] -> MixableList
itemListToSeq =  DS.fromList

showItems :: DS.Seq Item -> String
showItems itemSeq = show $ map (\(Item (ItemValue num) _) -> num) (F.toList itemSeq)

mix :: (MixableList, [Item]) -> (MixableList, [Item])
mix (items, []) = (items, [])
-- if the value is 0, do nothing
mix (items, (Item (ItemValue 0) _):xs) = mix (items, xs)
mix (items, x@(Item (ItemValue shiftValue) _):xs)
    -- if the value is a multiple of the length of the list
    -- do nothing
    | shiftValue `mod` length items == 0 = mix (items, xs)
    | otherwise = case dir of
        Left ->
            let targetDest = startIndex - moddedShiftValue
                insertIndex = targetDest - 1
                replaceItem = fromJust $ DS.lookup insertIndex items
            in
                if insertIndex < 0
                then mix (deleteWrapper (
                    "minus then, startIndex" ++ show startIndex
                    ++ " targetDest:" ++ show targetDest
                    ++ "shift value:" ++ show shiftValue ++ show " modded:" ++ show moddedShiftValue
                    ++ " insertIndex:" ++ show (numItems - abs targetDest)
                    ++ " processing item:" ++ show x
                    ) startIndex (DS.insertAt (numItems - abs targetDest) x items), xs)
                else mix (DS.insertAt (insertIndex + 1) (trace ("minus replace item:" ++ show replaceItem) replaceItem) (deleteWrapper "minus else" startIndex (DS.update insertIndex x items)), xs)
        Right ->
            let targetDest = startIndex + moddedShiftValue
                insertIndex = targetDest + 1
                replaceItem idx = fromJust $ DS.lookup (trace ("replace idx:" ++ show idx) idx) items
            in
                if insertIndex > lastIndex
                then
                    let newInsertIndex = insertIndex - numItems
                    in mix (DS.insertAt (newInsertIndex + 1) (let ri = replaceItem newInsertIndex in trace ("plus replace item" ++ show ri) ri) (deleteWrapper "right then" startIndex (DS.update newInsertIndex x items)), xs)
                else mix (deleteWrapper "right else" startIndex (DS.insertAt insertIndex x items), xs)
        where
            dir = if shiftValue > 0 then Right else Left
            moddedShiftValue = abs shiftValue `mod` numItems
            startIndex =
                fromJust (
                    if isNothing startIndexMaybe
                    then trace ("couldn't find item:"
                        ++ show x
                        ++ " num items:"
                        ++ show numItems
                        ++ " items:"
                        ++ show items) startIndexMaybe 
                    else startIndexMaybe
                )
            startIndexMaybe = DS.elemIndexL x items
            lastIndex = numItems - 1
            numItems = length items

deleteWrapper :: String -> Int -> MixableList -> MixableList
deleteWrapper debugMsg idx itemSeq =
    case itemSeq DS.!? idx of
        Nothing -> trace ("couldn't find item at idx:" ++ show idx ++ " to delete") afterDeletion
        Just item -> trace ("deleting item:" ++ show item ++ " " ++ debugMsg) afterDeletion
    where
        afterDeletion = DS.deleteAt idx itemSeq

wrapIndex :: Int -> Int -> Int
wrapIndex numItems idx
    | idx == 0 = idx
    | idx > numItems - 1 = idx `mod` numItems
    | otherwise = numItems - abs idx

--insertItem :: DS.Seq Item -> Int -> Item -> InsertDir -> DS.Seq Item
--insertItem items 0 item Left = DS.insertAt (length items - 1) item (DS.deleteAt
--insertItem items insertLoc item dir
--    | otherwise =
--        case dir of
--            Left -> case DS.lookup insertLoc of
--                Nothing -> undefined
--                Just shovedItem ->
--                    let seqWithPlacedItem = DS.adjust' (const item) insertLoc items
--                    in DS.insertAt (insertLoc + 1) shovedItem
--            Right -> undefined
--        where
--            seqLength = length items
--            adjustEdge finalIndex =
--                if finalIndex == 0 && isMinus
--                then numItems - 1
--                else if finalIndex == numItems - 1 && not isMinus
--                    then 0
--                    else finalIndex

--nextIndex :: Int -> Item -> Int -> Int
--nextIndex numItems (Item (ItemValue move) _) currIdx
--    | move == 0 || move `mod` numItems == 0 = currIdx
--    | move > 0 =
--        adjustEdge ((currIdx + (move `mod` numItems)) `mod` numItems) + (if didWrap then 1 else 0)
--    | otherwise =
--        let moveAmount = (abs move `mod` numItems)
--            indexAfterOffset = (currIdx - moveAmount)
--            finalIndex =
--                if indexAfterOffset < 0
--                then numItems + indexAfterOffset
--                else adjustEdge (indexAfterOffset `mod` numItems)
--        in myTrace
--            ("moving:" ++ show move ++ " final index:" ++ show finalIndex)
--            finalIndex + (if didWrap then (-1) else 0)
--    where
--        -- if it wraps around the list to the right or left, it's position will need
--        -- to be adjusted such that it ends up to the right of the element occupying
--        -- that space in the '+' case, and needs moved one to the left in the
--        -- '-' case
--        didWrap = let nextIdx = currIdx + move in nextIdx < 0 || nextIdx >= numItems
--        isMinus = move < 0

parseInput :: P.Parsec String () [Int]
parseInput = P.many (integer <* P.endOfLine) <* P.eof
