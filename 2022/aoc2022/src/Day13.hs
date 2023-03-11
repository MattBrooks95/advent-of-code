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
    , getState
    , modifyState
    )

data ParserState = ParserState {
    pairIndex :: Int
    }

-- I want this to be generic but I'm not sure how to do that
-- when I'm using digits to parse numbers
data List = NestedList [List] | NumericVal Int | EmptyList
    deriving (Show)

data Signal = Signal { contents::List }
    deriving (Show)
data SignalPair = SignalPair Int (Signal, Signal)
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
    parseResults <- (nestedList <|> emptyList) <* endOfLine
    return Signal { contents=parseResults }


parseSignalPair :: Parsec String ParserState SignalPair
--parseSignalPair = SignalPair <$> parseSignal `sepBy` endOfLine
parseSignalPair = do
    signalOne <- parseSignal
    signalTwo <- parseSignal
    ParserState currIndex <- getState
    modifyState (\(ParserState prevIndex) -> ParserState (prevIndex + 1))
    
    return $ SignalPair currIndex (signalOne, signalTwo)

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
parseProblem = Decoder <$> parseSignalPair `sepBy1` endOfLine <* eof
--parseProblem = Decoder <$> many parseSignalPair <* eof

run :: FilePath -> IO ()
run filePath = do
    input <- readFile filePath
    print input
    case runParser parseProblem (ParserState { pairIndex=1 }) filePath input of
        Right problem -> print problem
        Left e -> print e
   --print inputLines
    --let linesGrouped = groupLines inputLines
    --mapM_ print linesGrouped

compareList :: List -> List -> Bool
compareList (NestedList (x:xs)) (NestedList (y:ys)) = True --TODO map compare list over the item pairs of the two lists and do an and?
compareList (NestedList (_:_)) (NestedList []) = False
compareList (NestedList []) (NestedList (_:_)) = True
compareList (NestedList []) (NestedList []) = True
compareList EmptyList EmptyList = True
compareList EmptyList (NumericVal _) = False --can this case happen?
compareList (NumericVal _) EmptyList = False --can this case happen?
compareList l1@(NumericVal _) l2@(NestedList _) = compareList (NestedList [l1]) (NestedList [l2])
compareList l1@(NestedList _) l2@(NumericVal _) = compareList l1 (NestedList [l2])
compareList (NestedList _) EmptyList = False
compareList EmptyList (NestedList _) = True
compareList (NumericVal leftNum) (NumericVal rightNum) =
    leftNum <= rightNum
