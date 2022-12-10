module Day3 where

import Data.List (
    intersect
    , nub
    )

import Data.Char (
    ord
    )

import qualified Data.Map as M (
    fromList
    , Map(..)
    , lookup
    )

import Data.Maybe (
    fromMaybe
    , catMaybes
    )

import Data.Either

import Lib (
    chunk
    )

import Data.IntMap (intersection)

data Rucksack = Rucksack String String deriving (Show)

parseRucksack :: String -> Rucksack
parseRucksack [] = Rucksack [] []
parseRucksack chars = Rucksack leftItems rightItems
    where
        listLength = length chars
        halfListLength = listLength `div` 2
        leftItems = take halfListLength chars
        rightItems = drop halfListLength chars

findSameItems :: Rucksack -> (Rucksack, [Char])
findSameItems sack@(Rucksack left right) = (sack, dupes)
    where
        dupes = nub left `intersect` right

-- use list comprehensions to make the map for looking up
-- what priority value is given to each character
-- lowercase letters are values 97~122 in the ascii table
-- uppercase letters are 65~90
priorities :: [(Char, Int)]
priorities = lowerCasePriorities ++ upperCasePriorities
    where
        lowerCasePriorities = [(x, ord x - 96) | x <- lowerCaseLetters]
        upperCasePriorities = [(x, ord x - 38) | x <- upperCaseLetters]
        upperCaseLetters = ['A'..'Z']
        lowerCaseLetters = ['a'..'z']

prioritiesMap :: M.Map Char Int
prioritiesMap = M.fromList priorities

getMatchingItemsForRucksacks :: [Rucksack] -> Either String String
getMatchingItemsForRucksacks [] = Left "empty list"
getMatchingItemsForRucksacks [Rucksack xl xr, Rucksack yl yr, Rucksack zl zr] = Right $ intersect ((xl++xr) `intersect` (yl++yr)) (zl++zr)
getMatchingItemsForRucksacks _ = Left "list does not contain 3 elements"

dayThree :: [String] -> IO ()
dayThree inputLines = do
    let rucksacks = map parseRucksack inputLines
    print rucksacks
    let rucksacksWithDupes = map findSameItems rucksacks
    --mapM_ print rucksacksWithDupes
    --let rucksacksWithDupes = map (\r@(Rucksack left right) -> (r, findDuplicates left right)) rucksacks
    --mapM_ print rucksacksWithDupes
    let dupes = map (head . snd) rucksacksWithDupes
    let dupePriorities = foldr (\item acc -> acc + fromMaybe 0 (M.lookup item prioritiesMap)) 0 dupes :: Int
    print dupePriorities
    let rucksackGroups = chunk 3 rucksacksWithDupes
    case rucksackGroups of
        Left err -> print err
        Right groups -> do
            let badgesForGroup = map (\x -> getMatchingItemsForRucksacks (map fst x)) groups
            let prioritiesForBadges = map (\x -> M.lookup (head x) prioritiesMap) (rights badgesForGroup)
            let prioritiesSum = sum (catMaybes prioritiesForBadges)
            print prioritiesSum
            --mapM_ (putStrLn . \(x:_) -> "badgeLetter: " ++ x ++ " priority:" ++ (rights badgesForGroup)

