module Day1 where

import qualified Data.List as L (
    sortBy
    )
import Data.Ord (
    Down(..)
    )

import System.Exit (
    exitFailure
    )

import Data.List (sortBy)

import Data.Maybe (
    mapMaybe
    , fromJust
    )
import Text.Read (
    readMaybe
    )
import Lib (
    groupLines
    )

data Elf = Elf { calories::[Int] } deriving (Show)

getElfTotalCalories :: Elf -> Int
getElfTotalCalories elf = sum (calories elf)

findElfWithHighestCalories :: [Elf] -> Maybe (Elf, Int)
findElfWithHighestCalories [] = Nothing
findElfWithHighestCalories elves = Just $ head $ sortElvesByHighestCalories (elvesWithTotalCalories elves)
--findElfWithHighestCalories elves = L.sortBy  (\x y -> max (snd x) (snd y)) elvesWithTotalCalories
    --where
    --    elvesWithTotalCalories = map (\x -> (x, getElfTotalCalories x)) elves :: [(Elf, Int)]

elvesWithTotalCalories :: [Elf] -> [(Elf, Int)]
elvesWithTotalCalories elves = map (\x -> (x, getElfTotalCalories x)) elves :: [(Elf, Int)]

sortElvesByHighestCalories :: [(Elf, Int)] -> [(Elf, Int)]
sortElvesByHighestCalories elvesWithCalories = L.sortBy(\(_, a) (_, b) -> compare (Down a) (Down b)) elvesWithCalories

parseElf :: [String] -> Maybe Elf
parseElf [] = Nothing
parseElf inputStrings = Just Elf { calories = elfCalories }
    where
        --elfCalories = catMaybes $ map (readMaybe :: Int) inputStrings
        elfCalories = mapMaybe readMaybe inputStrings :: [Int]

dayOne :: [String] -> IO()
dayOne [] = print "error, no file contents"
dayOne lines = do
    let groupedLines = groupLines lines
    --print "grouped lines:"
    --print groupedLines
    let parsedElves = map parseElf groupedLines
    let elves = map fromJust (filter (\x -> case x of { Just e -> True; _ -> False }) parsedElves)
    -- part1
    print $ findElfWithHighestCalories elves
    -- part2
    let elvesCaloriesDescending = sortElvesByHighestCalories $ elvesWithTotalCalories elves
    if length elvesCaloriesDescending > 3
        then do
            let three = take 3 elvesCaloriesDescending
            print "top 3 elves:"
            print three
            let sumOfTopThree = sum $ map snd(take 3 elvesCaloriesDescending)
            print sumOfTopThree 
        else do
            print "can't find top 3 elves with the most calories, not enough elves"
            exitFailure

