module Day13
    --(
    --run
    --)
    where

import Debug.Trace
    (
    trace
    )

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
data List = NestedList [List] | NumericVal Int
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
emptyList = NestedList[] <$ string "[]"

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
    parseResults <- nestedList <* endOfLine
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
        Right (Decoder problem) -> do
            print problem
            let results = map (\(SignalPair idx (sig1, sig2)) -> (idx, compareList (contents sig1) (contents sig2))) problem
            print results
            let correctPairSum = sum $ map fst (filter (isCorrect . snd) results)
            print correctPairSum
        Left e -> print e
   --print inputLines
    --let linesGrouped = groupLines inputLines
    --mapM_ print linesGrouped

data Result = Incorrect | Continue | Correct deriving (Show, Eq)
isCorrect = (== Correct)

numCompare :: Int -> Int -> Result
numCompare l r
    | l < r = Correct
    | l == r = Continue
    | otherwise = Incorrect

--I think it isn't working because of the parse result
--sometimes the answer is wrapped in a NestedList one more time than it should be
compareList :: List -> List -> Result
--TODO map compare list over the item pairs of the two lists and do an and?
--gives 11 because pair2 becomes false when it shouldn't be
--compareList (NestedList (x:xs)) (NestedList (y:ys)) = compareList x y && compareListLengths xs ys
-- gives 18 because the 7s case becomes true...
--compareList (NestedList (x:xs)) (NestedList (y:ys)) = compareList x y && and (map (\(l1, l2) -> compareList l1 l2) (zip xs ys))
compareList (NestedList []) (NestedList []) = Correct
compareList (NestedList []) (NestedList (_:_)) = Correct
compareList (NestedList (_:_)) (NestedList []) = Incorrect
compareList (NumericVal lVal) (NumericVal rVal) = let res = numCompare lVal rVal in trace (show lVal ++ " vs " ++ show rVal ++ " " ++ show res) res
compareList lVal@(NumericVal _) rList@(NestedList _) = compareList (NestedList [lVal]) rList
compareList lList@(NestedList _) rVal@(NumericVal _) = compareList lList (NestedList [rVal])
compareList (NestedList (x:xs)) (NestedList (y:ys)) = case compareList x y  of
    Correct -> Correct
    Incorrect -> Incorrect
    Continue -> compareListLengths xs ys

--
--compareList (NestedList (_:_)) (NestedList []) = False
--compareList (NestedList []) (NestedList (_:_)) = True
--compareList (NestedList []) (NestedList []) = True
--compareList l1@(NumericVal _) l2@(NestedList _) = compareList (NestedList [l1]) l2
--compareList l1@(NestedList _) l2@(NumericVal _) = compareList l1 (NestedList [l2])
--compareList (NumericVal leftNum) (NumericVal rightNum) =
--    leftNum <= rightNum


compareListLengths :: [List] -> [List] -> Result
compareListLengths l1 l2 =
    if Correct `elem` listCheckResult then Correct
    else
        if all (==Continue) listCheckResult && length1 < length2 then Correct
        else if length1 == length2 then Continue else Incorrect
    --else Continue
    --trace (show (l1, l2, answer)) answer
    where
        --answer = (Correct `elem` listCheckResult) && length1 <= length2
        length1 = length l1
        length2 = length l2
        listCheckResult = map (\(li1, li2) -> compareList li1 li2) (zip l1 l2)

--map (\(l1, l2) -> compareList l1 l2
