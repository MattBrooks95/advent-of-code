module Day1 where

import qualified Data.List as L (
    sortBy
    )
import Data.Ord (
    Down(..)
    )
import Data.List (sortBy)

data Elf = Elf { calories::[Int] }

getElfTotalCalories :: Elf -> Int
getElfTotalCalories elf = sum (calories elf)

findElfWithHighestCalories :: [Elf] -> Maybe Elf
findElfWithHighestCalories [] = Nothing
findElfWithHighestCalories elves = Just $ fst $ head $ sortElvesByHighestCalories elvesWithTotalCalories
--findElfWithHighestCalories elves = L.sortBy  (\x y -> max (snd x) (snd y)) elvesWithTotalCalories
    where
        elvesWithTotalCalories = map (\x -> (x, getElfTotalCalories x)) elves :: [(Elf, Int)]

sortElvesByHighestCalories :: [(Elf, Int)] -> [(Elf, Int)]
sortElvesByHighestCalories elvesWithCalories = L.sortBy(\(_, a) (_, b) -> compare (Down a) (Down b)) elvesWithCalories
