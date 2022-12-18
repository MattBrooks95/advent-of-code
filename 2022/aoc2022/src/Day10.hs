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

import Lib (
    chunk
    )

run :: [String] -> IO ()
run inputLines = do
    --print inputLines
    let parseFailures = lefts actions
    print $ "num parse failures:" ++ show (length parseFailures)
    mapM_ print parseFailures
    let parseSuccesses = rights actions
    print $ "num instructions parsed:" ++ show (length parseSuccesses)
    let instructions = map makeInstruction parseSuccesses
    --print "parse successes:"
    --mapM_ print parseSuccesses 
    --putStrLn "instructions:"
    --print instructions
    let simResult = runProgram instructions 1 1
    if length simResult > 220 then do
        print $ take 20 $ drop 210 simResult
        print "special cycles:"
        let specialCycles = simResult !! 19:[ simResult !! (takeCycle - 1) | takeCycle <- [60, 100..220]]
        mapM_ print specialCycles
        let signalStrengths = map calcSignalStrength specialCycles
        print "signal strengths:"
        print $ show (sum signalStrengths)
        drawCrt simResult
    else
        print simResult
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
        then [(tick, register, thisIns), (nextTick, newRegValue, thisIns)]
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

calcSignalStrength :: (Int, Register, Instruction) -> Int
calcSignalStrength (tick, register, _) = tick * register

drawCrt :: [(Int, Register, Instruction)] -> IO ()
drawCrt tickInfo = do
    let numTicks = length tickInfo
    print $ "drawCrt, num ticks:" ++ show numTicks
     -- there are 241 inputs, my chunk function only works if it divides evenly
     -- the crt won't draw that 241st frame so, hopefully throwing it away is okay
    let ticksChunked = chunk 40 (init tickInfo)
    case ticksChunked of
        Left msg -> print msg
        Right chunked -> do
            --mapM_ print chunked
            let crtDisplayString = drawLine (init tickInfo)
            putStrLn crtDisplayString
            case chunk 40 crtDisplayString of
                Left err -> putStrLn err
                -- the function chunk that ordered the symbols into groups of 40
                -- reversed the groups and their contents, so they need reversed again
                Right chunked' -> mapM_ (putStrLn . reverse) (reverse chunked')

drawLine :: [(Int, Register, Instruction)] -> String
drawLine ticks = drawLine' ticks 0

drawLine' :: [(Int, Register, Instruction)] -> Int -> String
drawLine' [] _ = []
drawLine' ((_, regVal, _):ticks) crtTick = 
    let symbol = if rangeStart <= moddedCrtTick && moddedCrtTick <= rangeEnd
        then '#'
        else '.'
        in
        symbol:drawLine' ticks (crtTick + 1)
    --if rangeStart >= moddedCrtTick && moddedCrtTick <= rangeEnd
    --then '#':drawLine' ticks (crtTick + 1)
    --else '.':drawLine' ticks (crtTick + 1)
    where
        moddedCrtTick = crtTick `mod` 40
        rangeStart = regVal - 1
        rangeEnd = regVal + 1

