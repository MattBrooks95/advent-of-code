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

data Location = Location Int Int

--data Sand = Sand { settled::Bool, location::Location }

data Space = Empty | Rock { location::Location } | Sand { settled::Bool, location::Location }

run :: FilePath -> IO()
run fp = do
    fileContents <- ST.readFile fp
    print fileContents
    print "day 14"
    case runParser parse () fp fileContents of
        Right rockPaths -> do
            mapM_ print rockPaths
        Left parseError -> print parseError

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
