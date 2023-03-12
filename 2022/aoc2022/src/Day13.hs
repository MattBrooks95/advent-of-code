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
instance Show List where
    show (NestedList []) = "[]"
    show (NestedList list) = show list
    show (NumericVal num) = show num

data Signal = Signal { contents::List }
instance Show Signal where
    show (Signal {contents=sig}) = show sig
data SignalPair = SignalPair Int (Signal, Signal)
instance Show SignalPair where
    show (SignalPair idx (s1, s2)) = "\n(Pair idx:"++show idx++"\n"++show s1++"\n"++show s2++")\n"

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

--5832 too high
--5649 too high
--5520 wrong
--5544 wrong

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

test :: IO ()
test = do
    --let tests = []
    --I guess the lists being totally equal is impossible
    let tests = [
            (NestedList [], NestedList [], Continue)
            , (NestedList [NumericVal 1], NestedList [NumericVal 1], Continue)
            , (NestedList [NumericVal 2], NumericVal 2, Continue)
            , (NestedList [NumericVal 1], NestedList [NestedList []], Incorrect)
            , (NestedList [NestedList []], NestedList [NumericVal 1], Correct)
            , (NumericVal 1, NumericVal 2, Correct)
            , (NumericVal 2, NumericVal 1, Incorrect)
            , (NestedList [NestedList[], NestedList[]], NestedList[NestedList []], Incorrect)
            , (NestedList [NumericVal 1, NumericVal 2], NestedList [NumericVal 1, NumericVal 2], Continue)
            , (NestedList [NestedList []], NestedList [NestedList []], Continue)
            , (NestedList [NumericVal 1], NestedList [NestedList [NumericVal 1]], Continue)
            , (NestedList [NumericVal 2], NestedList [NestedList [NumericVal 2], NumericVal 3], Correct)
            , (NestedList [NestedList [NumericVal 2]], NestedList [NestedList [NumericVal 2]], Continue)
            , (NestedList [NestedList [NumericVal 2]], NestedList [NumericVal 2], Continue)
            , (NestedList [NestedList [NumericVal 2], NumericVal 3], NestedList [NumericVal 2], Incorrect)
            , (NestedList [NestedList [NumericVal 2], NumericVal 3], NestedList [NestedList [NumericVal 2]], Incorrect)
            ]
    let testResults = zip ([1..] :: [Int]) (map runTest tests)
    mapM_ print testResults
    print ("num failures:" ++ show (length $ filter (\(_, TestResult (_, _, passed)) -> not passed) testResults))

newtype TestResult = TestResult (Result, Result, Bool)
instance Show TestResult where
    show (TestResult (expected, actual, testPassed)) = "Actual:"++show expected++" Expected:"++show actual++" succeded?:"++show testPassed

runTest :: (List, List, Result) -> TestResult
runTest (l1, l2, answer) = TestResult (result, answer, answer == result)
    where result = compareList l1 l2

data Result = Incorrect | Continue | Correct deriving (Show, Eq)
isCorrect :: Result -> Bool
isCorrect = (== Correct)

numCompare :: Int -> Int -> Result
numCompare l r
    | l < r = Correct
    | l == r = Continue
    | otherwise = Incorrect

--compareLists :: [List] -> [List] -> Result
--compareLists 

compareList :: List -> List -> Result
compareList (NestedList []) (NestedList []) = Continue
compareList (NestedList []) (NestedList (_:_)) = Correct
compareList (NestedList (_:_)) (NestedList []) = Incorrect
--compareList (NumericVal lVal) (NumericVal rVal) = let res = numCompare lVal rVal in trace (show lVal ++ " vs " ++ show rVal ++ " " ++ show res) res
compareList (NumericVal lVal) (NumericVal rVal) = let res = numCompare lVal rVal in res
--compareList (NumericVal lVal) (NumericVal rVal) = let res = numCompare lVal rVal in res
compareList lVal@(NumericVal _) rList@(NestedList _) = compareList (NestedList [lVal]) rList
compareList lList@(NestedList _) rVal@(NumericVal _) = compareList lList (NestedList [rVal])
--compareList (NestedList (x:xs)) (NestedList (y:ys)) = case trace (show $ "compare head l:" ++ show x ++ " r:" ++ show y ++ " " ++ show itemCompareResult) itemCompareResult  of
compareList (NestedList (x:xs)) (NestedList (y:ys)) = case itemCompareResult  of
    Correct -> Correct
    Incorrect -> Incorrect
    -- it was necessary to just re-wrap what was left after the head comparison and continue
    -- to recurse, instead of using the compareListLengths helper
    Continue -> compareList (NestedList xs) (NestedList ys)
    --Continue -> compareListLengths xs ys
    where
        itemCompareResult = compareList x y

--compareListLengths :: [List] -> [List] -> Result
--compareListLengths [] [] = Continue
--compareListLengths l1 l2
--    | Correct `elem` loggedResult = Correct
--    | all (==Continue) loggedResult && length1 < length2 = Correct
--    | all (==Continue) loggedResult && length1 == length2 = Continue
--    -- this case was necessary for it to 'short-circuit' by stopping processing
--    -- the moment an incorrect result was found
--    | Incorrect `elem` loggedResult = Incorrect
--    | otherwise = Incorrect
--    --else Continue
--    --trace (show (l1, l2, answer)) answer
--    where
--        --answer = (Correct `elem` listCheckResult) && length1 <= length2
--        length1 = length l1
--        length2 = length l2
--        loggedResult = trace ("compareListLengths:" ++ show listCheckResult) listCheckResult
--        listCheckResult = map (\(li1, li2) -> compareList li1 li2) (zip l1 l2)
