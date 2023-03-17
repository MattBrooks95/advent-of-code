module Day14
    --(
    --run
    --)
    where

import System.IO.Strict as ST (
    readFile
    )

import Text.Parsec (
    sepBy
    , sepBy1
    , Parsec
    , digit
    , char
    , many1
    , spaces
    , endOfLine
    , many
    , string
    , runParser
    , eof
    , skipMany
    , between
    , try
    )

import qualified Data.Set as S

data Location = Location Int Int deriving (Show)

--data Sand = Sand { settled::Bool, location::Location }

newtype RockPath = RockPath [Location] deriving Show
data Sand = Sand { settled::Bool, location::Location } deriving (
    Show
    )

run :: FilePath -> IO()
run fp = do
    fileContents <- ST.readFile fp
    print fileContents
    print "day 14"
    case runParser parse () fp fileContents of
        Right parsedRps -> do
            mapM_ print parsedRps
            let rockpaths = map generateRockPath parsedRps
            print rockpaths
        Left parseError -> print parseError

generateRockPath :: [(Int, Int)] -> RockPath
generateRockPath [] = RockPath []
generateRockPath pathPoints =
    RockPath $ foldl generateIntermediatePoints [] pathPoints

generateIntermediatePoints :: [Location] -> (Int, Int) -> [Location]
generateIntermediatePoints [] (ex, ey) = [Location ex ey]
generateIntermediatePoints prevPoints@(_:_) (ex, ey) = let Location sx sy = last prevPoints in
    if sx /= ex then prevPoints ++ [ Location x sy | x <- generatePointsBetween sx ex ]
    --if sx /= ex then prevPoints ++ drop 1 [ Location x sy | x <- [sx..ex] ]
    else if sy /= ey then prevPoints ++ [ Location sx y | y <- generatePointsBetween sy ey ]
    else []


generatePointsBetween :: Int -> Int -> [Int]
generatePointsBetween num1 num2
    -- | abs (num2 - num1) == 1 = []
    | num1 < num2 = reverse $ (num1 + 1):generatePointsBetween (num1 + 1) num2
    | num2 < num1 = reverse $ (num2 + 1):generatePointsBetween num1 (num2+1)
    | otherwise = []



comma :: Parsec String () Char
comma = char ','

digits :: Parsec String () Int
digits = read <$> many1 digit

plainWhitespace :: Parsec String () Char
plainWhitespace = char ' '

numberPair :: Parsec String () (Int, Int)
numberPair = do
    digit1 <- digits
    _ <- comma
    digit2 <- digits
    return (digit1, digit2)

sepArrow :: Parsec String () String
sepArrow = between ws ws (string "->")
    where
        ws = many plainWhitespace

parseRockPath :: Parsec String () [(Int, Int)]
parseRockPath = numberPair `sepBy1` sepArrow

parse :: Parsec String () [[(Int, Int)]]
parse = many (parseRockPath <* endOfLine) <* eof
