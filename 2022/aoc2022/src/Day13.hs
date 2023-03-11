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
    , many1
    , endOfLine
    , try
    , string
    , choice
    , sepBy
    , sepBy1
    , many
    , digit
    , (<|>)
    , runParser
    , optional
    , between
    )

data ParserState = ParserState {
    }

-- I want this to be generic but I'm not sure how to do that
-- when I'm using digits to parse numbers
data List = NestedList [List] | NumericVal Int | EmptyList
    deriving (Show)

data Signal = Signal { index::Int, contents::List }
    deriving (Show)
newtype SignalPair = SignalPair (Signal, Signal)
    deriving (Show)

newtype Decoder = Decoder [SignalPair]
    deriving (Show)

openB :: Parsec String ParserState Char
openB = char '['
closeB :: Parsec String ParserState Char
closeB = char ']'

emptyList :: Parsec String ParserState List
emptyList = EmptyList <$ string "[]"

numericVal :: Parsec String ParserState List
numericVal = NumericVal <$> (Prelude.read <$> many1 digit)
--listOfDigits :: Parsec String ParserState [Int]
--listOfDigits = fmap Prelude.read <$> many1 digit `sepBy` char ','

--atomList :: Parsec String ParserState List
--atomList = AtomList <$> (openB *> listOfDigits <* closeB)

nestedList :: Parsec String ParserState List
nestedList = NestedList <$> between openB closeB (choice [nestedList, emptyList, numericVal] `sepBy` char ',')

parseSignal :: Parsec String ParserState Signal
parseSignal = do
    parseResults <- (emptyList <|> nestedList) <* endOfLine
    return Signal { index=0, contents=parseResults }


parseSignalPair :: Parsec String ParserState SignalPair
--parseSignalPair = SignalPair <$> parseSignal `sepBy` endOfLine
parseSignalPair = SignalPair <$> do
    signalOne <- parseSignal
    signalTwo <- parseSignal
    return (signalOne, signalTwo)

--parseSignalPair = do
    --instead of matching a list and then trying to assert that there are
    --only two, write the parser such that it only succeeds on reading 2 signals
    --separated by a newline
    --but the hard part is putting the result into a tuple, like I want to
    --signals <- parseSignal `sepBy` endOfLine
    --case signals of
    --    [sig1, sig2] -> return $ SignalPair (sig1, sig2)
    --    _ -> Parse

parseProblem :: Parsec String ParserState Decoder
parseProblem = Decoder <$> parseSignalPair `sepBy` endOfLine <* eof
--parseProblem = Decoder <$> many parseSignalPair <* eof

run :: FilePath -> IO ()
run filePath = do
    input <- readFile filePath
    print input
    case runParser parseProblem (ParserState {}) filePath input of
        Right problem -> print problem
        Left e -> print e
   --print inputLines
    --let linesGrouped = groupLines inputLines
    --mapM_ print linesGrouped

