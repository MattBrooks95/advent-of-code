module Day10 where

import Debug.Trace

import Data.Either (
    lefts
    , rights
    , Either(..)
    )

import Data.Text (
    splitOn
    , pack
    , unpack
    )

run :: [String] -> IO ()
run inputLines = do
    --print inputLines
    let parseFailures = lefts actions
    print $ "num parse failures:" ++ show (length parseFailures)
    mapM_ print parseFailures
    let parseSuccesses = rights actions
    let instructions = map makeInstruction parseSuccesses
    --print "parse successes:"
    --mapM_ print parseSuccesses 
    --putStrLn "instructions:"
    --print instructions
    let simResult = runProgram instructions 1 0
    --print simResult
    print "special cycles:"
    mapM_ print ([simResult !! 20] ++ [ simResult !! takeCycle | takeCycle <- [60, 100..220]])
    where
        actions = map parse inputLines

data Action = Noop | AddX Int deriving (Show)
data Instruction = Instruction Action Int deriving (Show)
--remainingTicks (Instruction _ ticks) = ticks

type Register = Int

runProgram :: [Instruction] -> Register -> Int -> [(Int, Register, Instruction)]
runProgram [] _ _ = []
runProgram (thisIns@(Instruction action remainingTicks):inss) register tick = case remainingTicks of
    1 ->
        if null inss
        then [(nextTick, newRegValue, thisIns)]
        else (tick, register, thisIns):runProgram inss newRegValue nextTick
            where
                newRegValue = applyAction register action
    ticksLeft -> (tick, register, thisIns):runProgram (Instruction action (ticksLeft - 1):inss) register nextTick
    where
        nextTick = tick + 1

makeInstruction :: Action -> Instruction
makeInstruction Noop = Instruction Noop 1
makeInstruction (AddX addVal) = Instruction (AddX addVal) 2

parse :: String -> Either String Action
parse ln = let parts = splitLine in
--parse ln = let parts = trace (show splitLine) splitLine in
    if null parts then Left "split on spaces failure"
    else if (unpack . head) parts == "noop" then Right Noop
    else Right (AddX ((read . unpack . last) parts :: Int))
    where
        splitLine = (splitOn (pack " ") . pack) ln

applyAction :: Register -> Action -> Register
applyAction reg Noop = reg
applyAction reg (AddX addVal) = reg + addVal
