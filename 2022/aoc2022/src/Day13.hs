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

data List a = NestedList [List a] | AtomList [a] | EmptyList
    deriving (Show)

data Signal a = Signal { index::Int, contents::List a }
newtype SignalPair a = SignalPair (Signal a, Signal a)

newtype Decoder a = Decoder [SignalPair a]

openB :: Parsec String ParserState Char
openB = char '['
closeB :: Parsec String ParserState Char
closeB = char ']'

emptyList :: Parsec String ParserState (List a)
emptyList = EmptyList <$ string "[]"

listOfDigits :: Parsec String ParserState [Int]
listOfDigits = fmap Prelude.read <$> many digit `sepBy` char ','

atomList :: Parsec String ParserState (List Int)
atomList = AtomList <$> listOfDigits

--nestedList :: Parsec String ParserState (List a)
--nestedList = NestedList $ openB *> many nestedList <* closeB
--
--parseSignal :: Parsec String ParserState (Signal a)
--parseSignal = openB *> choice [emptyList, nestedList] *< closeB
--
--parseSignalPair :: Parsec String ParserState (SignalPair a)
--parseSignalPair = parseSignal <*> parseSignal <* endOfLine
--
--parseProblem :: Parsec String ParserState (Decoder a)
--parseProblem = many parseSignalPair <* eof

run :: FilePath -> IO ()
run filePath = do
    input <- readFile filePath
    print input
    --print inputLines
    --let linesGrouped = groupLines inputLines
    --mapM_ print linesGrouped

