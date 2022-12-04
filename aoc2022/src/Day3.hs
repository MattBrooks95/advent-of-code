module Day3 where

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
        dupes = findDuplicates left right

-- use list comprehensions to make the map for looking up
-- what priority value is given to each character
priorities :: [(Char, Int)]
priorities = [ (x, 0) | x <- lowerCase ++ upperCase]
    where
        upperCase = ['A'..'Z']
        lowerCase = ['a'..'z']
