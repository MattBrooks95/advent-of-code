module Day13
    --(
    --run
    --)
    where

import Debug.Trace
    (
    trace
    )

import Data.Maybe (
    fromJust
    )

import Data.List (
    sortBy
    , elemIndex
    , findIndex
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

nestedList :: Parsec String ParserState List
nestedList = NestedList <$> between openB closeB (choice [nestedList, emptyList, numericVal] `sepBy` char ',')

parseSignal :: Parsec String ParserState Signal
parseSignal = do
    parseResults <- nestedList <* endOfLine
    return Signal { contents=parseResults }


parseSignalPair :: Parsec String ParserState SignalPair
parseSignalPair = do
    signalOne <- parseSignal
    signalTwo <- parseSignal
    ParserState currIndex <- getState
    modifyState (\(ParserState prevIndex) -> ParserState (prevIndex + 1))

    return $ SignalPair currIndex (signalOne, signalTwo)

parseProblem :: Parsec String ParserState Decoder
parseProblem = Decoder <$> parseSignalPair `sepBy1` endOfLine <* eof
--parseProblem = Decoder <$> many parseSignalPair <* eof

--part one guesses:
--5832 too high
--5649 too high
--5520 wrong
--5544 wrong

run :: FilePath -> IO ()
run filePath = do
    input <- readFile filePath
    print input
    let parseResult = runParser parseProblem (ParserState { pairIndex=1 }) filePath input
    case parseResult of
        Right (Decoder problem) -> do
            print problem
            let results = map (\(SignalPair idx (sig1, sig2)) -> (idx, compareList (contents sig1) (contents sig2))) problem
            print results
            let correctPairSum = sum $ map fst (filter (isCorrect . snd) results)
            print correctPairSum
        Left e -> print e
    -- [[2]] (127) * [[6]] (215) = 27305 too low
    -- I forgot that the indices are 1-based, let's try
    -- [[2]] (128) * [[6]] (216) = 27648
    print "part2############"
    case parseResult of
        Right (Decoder problem) -> do
            -- split out the signal pairs into a single big list of signals
            let signalsInOneList = foldl (\acc (SignalPair _ (sig1, sig2)) -> sig1:sig2:acc) [] problem
            let dividerPackets@[divider1, divider2] = [NestedList [NestedList [NumericVal 2]], NestedList [NestedList [NumericVal 6]]]
            let finalList = zip (dividerPackets ++ map contents signalsInOneList) ([1..] :: [Int])
            mapM_ print (take 10 finalList)
            print $ "length of signals list:" ++ (show . length) finalList
            let sorted = sortBy (\(lst1, _) (lst2, _) -> compareListOrdering lst1 lst2) finalList
            print "sorted list:"
            mapM_ print sorted
            -- I wanted to use `elemIndex`, but I couldn't figure out how to derive Eq for the recursive List
            -- data structure, so I had to staple an index to them and look for that instead
            let packet1Index = 1 + fromJust (findIndex (\(_, signalId) -> signalId == 1) sorted)
            let packet2Index = 1 + fromJust (findIndex (\(_, signalId) -> signalId == 2) sorted)
            print $ "packet [[2]] index (1 based!!!):" ++ show packet1Index ++ " packet [[6]] index " ++ show packet2Index
            print $ "signal strength = " ++ show (packet1Index * packet2Index)
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

compareListOrdering :: List -> List -> Ordering
compareListOrdering l1 l2 = case compareList l1 l2 of
    Continue -> LT
    Correct -> LT
    Incorrect -> GT

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
