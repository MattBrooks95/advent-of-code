{-# LANGUAGE RecordWildCards #-}
module Day11 (
    run
    ) where

import Lib (
    groupLines
    , dropLeadingWhitespaceTxt
    , getItemFromList
    )

import Debug.Trace

import System.Exit

import Text.Regex.TDFA
import Text.Read (
    readMaybe
    )
import Data.List (
    intercalate
    --, sortBy
    , sortOn
    , sortBy
    , find
    , sort
    )
import Data.Maybe (
    isNothing
    , fromJust
    )
import Data.Either
import Data.Ord
import qualified Data.Text as T (
    pack
    , unpack
    , splitOn
    , Text
    )
import qualified Data.Map as M
--import Debug.Trace

run :: [String] -> IO ()
run inputLines = do
    --print inputLines
    let grouped = groupLines inputLines
    --mapM_ (putStrLn . concat) grouped
    let monkeys = map parseMonkey grouped
    if not $ null $ lefts monkeys then putStrLn $ "monkey parsing failures!:" ++ intercalate "\n" (lefts monkeys)
    else do
        print "parsed monkeys:"
        mapM_ print (rights monkeys)
        print "####################"
        let result@(monkeyList, countMap) = runSimulation (rights monkeys) 20
        print $ result
        let twoMostInspections = take 2 $ sortBy (\x y -> Down x `compare` Down y) (map snd (M.toList countMap))
        print $ "two mostInspections:" ++ show twoMostInspections
        print $ "monkey business:" ++ show (product twoMostInspections)

type ItemWorry = Int
type MonkeyId = Int
data Monkey = Monkey { mId::MonkeyId
    , items::[ItemWorry] 
    , inspectOperation::ItemWorry -> ItemWorry
    , test::ItemWorry -> Bool
    , onTrue::MonkeyId
    , onFalse::MonkeyId
    }

instance Show Monkey where
    show (Monkey { .. }) = intercalate "\n" [
        "monkeyId:" ++ show mId
        , "items:" ++ show items
        , "throw to monkey:" ++ show onTrue ++ " when condition is true"
        , "throw to monkey:" ++ show onFalse ++ " when condition is false"
        ]

-- outermost loop runs for 20 rounds
-- inner loop runs a function that simulates the inspections of the active monkey
-- this function returns (finishedInspectingMonkey, [rest of updated monkeys]
-- when the inner loop finishes, round count is increased by one
-- recurse, with the parameter list of monkeys being the (monkeyWhoFinishedInspectionLast, [list of update monkeys]
runSimulation :: [Monkey] -> Int -> ([Monkey], M.Map MonkeyId Int)
runSimulation [] _ = ([], M.empty)
runSimulation monkeys 0 = (monkeys, M.empty)
runSimulation monkeys roundCount = runSimulation' (sortOn mId monkeys) roundCount 0 (initMonkeyInspectCountMap monkeys)

initMonkeyInspectCountMap :: [Monkey] -> M.Map MonkeyId Int
initMonkeyInspectCountMap monkeys = M.fromList (map (\x -> (mId x, 0)) monkeys)

runSimulation' :: [Monkey] -> Int -> MonkeyId -> M.Map MonkeyId Int -> ([Monkey], M.Map MonkeyId Int)
runSimulation' monkeys 0 _ inspectCounts = (monkeys, inspectCounts)
runSimulation' monkeys roundCount activeMonkeyId inspectCounts =
    let (newActiveMonkey, otherMonkeys) = getItemFromList (\x -> mId x == activeMonkeyId) monkeys in
        case newActiveMonkey of
            Just m -> let (updatedActiveMonkey, updatedMonkeys, inspectionCount) = doInspectionLoop otherMonkeys m 0 in
                runSimulation' (sortOn mId $ updatedActiveMonkey:updatedMonkeys) roundCount (activeMonkeyId + 1) (M.adjust (+ inspectionCount) activeMonkeyId inspectCounts)
            Nothing -> runSimulation' monkeys (roundCount - 1) 0 inspectCounts

doInspectionLoop :: [Monkey] -> Monkey -> Int -> (Monkey, [Monkey], Int)
doInspectionLoop otherMonkeys activeMonkey@(Monkey { items=itemsList }) inspectCount = do
    -- TODO make sure inspect item is the new worry value
    if null itemsList then (activeMonkey, otherMonkeys, inspectCount)
    else
        let inspectItem = trace("monkey:" ++ show (mId activeMonkey) ++ " inspects item:" ++ show (head itemsList)) head itemsList in 
        let newInspectItemValue = lessenWorry (inspectOperation activeMonkey inspectItem) in
        let testResult = test activeMonkey newInspectItemValue in
        let targetMonkeyId = if testResult then onTrue activeMonkey else onFalse activeMonkey in
        let updateTargetMonkeyOperation = addItemToMonkey targetMonkeyId newInspectItemValue in
        doInspectionLoop (map updateTargetMonkeyOperation otherMonkeys) (activeMonkey { items=tail itemsList }) (inspectCount + 1) 

lessenWorry :: Int -> Int
lessenWorry worry = floor ((fromIntegral worry :: Double) / 3)

addItemToMonkey :: MonkeyId -> ItemWorry -> Monkey -> Monkey
addItemToMonkey targetMonkeyId itemVal monkey =
    if mId monkey == targetMonkeyId
    then trace ("add item:" ++ show itemVal ++ " to monkey:" ++ show (mId monkey)) (monkey { items=items monkey++[itemVal] })
    else monkey

parseMonkey :: [String] -> Either String Monkey
parseMonkey inputLines = do
    monkeyId <- parseMonkeyId $ head inputLines
    itemsList <- parseItemsList $ inputLines !! 1
    operation <- parseOperation $ inputLines !! 2
    inspectTest <- parseTest $ inputLines !! 3
    throwOnTrue <- parseTargetMonkeyId $ inputLines !! 4
    throwOnFalse <- parseTargetMonkeyId $ inputLines !! 5
    return Monkey {
        mId=monkeyId
        , items=itemsList
        , inspectOperation=operation
        , test=inspectTest
        , onTrue=throwOnTrue
        , onFalse=throwOnFalse
    }

parseMonkeyId :: String -> Either String MonkeyId
parseMonkeyId str = let splitResults = T.splitOn (T.pack " ") (T.pack str) in
    if length splitResults /= 2 then Left $ "monkey id line wasn't able to be split:" ++ str
    else Right (read (init ((T.unpack . last) splitResults)))

parseItemsList :: String -> Either String [ItemWorry]
parseItemsList str = let readInts = map readMaybe matches :: [Maybe Int] in
    if not $ null (filter isNothing readInts) then
        Left ("failed to parse out items list " ++ str ++ show matches)
    else
        Right (map fromJust readInts)
    where
        matches = getAllTextMatches (str =~ "[[:digit:]]+") :: [String]

parseOperation :: String -> Either String (ItemWorry -> ItemWorry)
parseOperation str = do
    matches <- if length firstSplitResult < 2 then Left ("not enough results after split:" ++ str) else Right firstSplitResult
    splitOp <- splitOperationLine (matches !! 1)
    operator <- case T.unpack $ splitOp !! 1 of
        "+" -> Right (+)
        "*" -> Right (*)
        failed -> Left ("couldn't parse operator:" ++ failed)
    let operand2ReadTarget = T.unpack (last splitOp)
    case readMaybe operand2ReadTarget :: Maybe Int of
        Just literalValue -> Right $ operator literalValue
        Nothing -> if operand2ReadTarget == "old"
            then Right (\x -> x `operator` x)
            else Left "bad operand 2"

    where
        firstSplitResult = T.splitOn (T.pack "=") (T.pack str)

splitOperationLine :: T.Text -> Either String [T.Text]
splitOperationLine str = let splitOp = T.splitOn (T.pack " ") (dropLeadingWhitespaceTxt str) in
    if length splitOp /= 3 then Left ("not enough matches:" ++ T.unpack str) else Right splitOp

parseTest :: String -> Either String (ItemWorry -> Bool)
parseTest str = let numberMatch = str =~ "[[:digit:]]+" :: String in
    --case readMaybe (trace ("matched test:" ++ numberMatch ++ " from input:" ++ str) numberMatch) :: Maybe Int of
    case readMaybe numberMatch :: Maybe Int of
        Just num -> Right (\x -> x `mod` num == 0)
        Nothing -> Left ("failed to parse test:" ++ str)

parseTargetMonkeyId :: String -> Either String MonkeyId
parseTargetMonkeyId str = case readMaybe (str =~ "[[:digit:]]+") :: Maybe Int of
    Just monkeyId -> Right monkeyId
    Nothing -> Left $ "couldn't get target monkeyId from:" ++ str
