{-# LANGUAGE RecordWildCards #-}
module Day11 (
    run
    ) where

import Lib (
    groupLines
    , dropLeadingWhitespaceTxt
    )

import Text.Regex.TDFA
import Text.Read (
    readMaybe
    )
import Data.List (
    intercalate
    )
import Data.Maybe (
    isNothing
    , fromJust
    )
import Data.Either
import qualified Data.Text as T (
    pack
    , unpack
    , splitOn
    )
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
        mapM_ print monkeys


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
parseOperation str = let matches = T.splitOn (T.pack "=") (T.pack str) in
    if length matches < 2
    then Left ("not enough matches:" ++ str)
    else let splitOp = T.splitOn (T.pack " ") (dropLeadingWhitespaceTxt (last matches)) in
        if length splitOp /= 3
        then Left ("operation split did not yield three results:" ++ ((T.unpack . last) matches))
        else let operator = splitOp !! 1 in
            let operand2 = read (T.unpack (last splitOp)) in
                case T.unpack operator of
                    "+" -> Right (+ operand2)
                    "*" -> Right (* operand2)
                    _ -> Left "bad operator"

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
    
