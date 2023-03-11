module Day13
    --(
    --run
    --)
    where

import Lib (
    groupLines
    )

import Text.Parsec (
    char
    , Parsec
    , eof
    , many
    , endOfLine
    , try
    , string
    , choice
    , sepBy
    , many
    , digit
    , (<|>)
    )

data ParserState = ParserState {
    }

-- I want this to be generic but I'm not sure how to do that
-- when I'm using digits to parse numbers
data List = NestedList [List] | AtomList [Int] | EmptyList
    deriving (Show)

data Signal = Signal { index::Int, contents::List }
newtype SignalPair = SignalPair (Signal, Signal)

newtype Decoder a = Decoder [SignalPair]

openB :: Parsec String ParserState Char
openB = char '['
closeB :: Parsec String ParserState Char
closeB = char ']'

emptyList :: Parsec String ParserState List
emptyList = EmptyList <$ string "[]"

listOfDigits :: Parsec String ParserState [Int]
listOfDigits = fmap Prelude.read <$> many digit `sepBy` char ','

atomList :: Parsec String ParserState List
atomList = AtomList <$> listOfDigits

nestedList :: Parsec String ParserState List
nestedList = openB *> (emptyList <|> NestedList <$> many nestedList) <* closeB

parseSignal :: Parsec String ParserState Signal
parseSignal = do
    parseResults <- openB *> choice [emptyList, atomList] <* closeB
    return Signal { index=0, contents=parseResults }


parseSignalPair :: Parsec String ParserState SignalPair
--parseSignalPair = SignalPair <$> parseSignal `sepBy` endOfLine
parseSignalPair = --do
    --instead of matching a list and then trying to assert that there are
    --only two, write the parser such that it only succeeds on reading 2 signals
    --separated by a newline
    --but the hard part is putting the result into a tuple, like I want to
    --signals <- parseSignal `sepBy` endOfLine
    --case signals of
    --    [sig1, sig2] -> return $ SignalPair (sig1, sig2)
    --    _ -> Parse

--parseProblem :: Parsec String ParserState (Decoder a)
--parseProblem = many parseSignalPair <* eof

run :: FilePath -> IO ()
run filePath = do
    input <- readFile filePath
    print input
    --print inputLines
    --let linesGrouped = groupLines inputLines
    --mapM_ print linesGrouped

