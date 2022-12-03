module Day1 where

import qualified Data.List as L (
    sortBy
    )
import Data.Ord (
    Down(..)
    )
import Data.List (sortBy)
import Data.Maybe (
    mapMaybe
    )
import Text.Read (
    readMaybe
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

