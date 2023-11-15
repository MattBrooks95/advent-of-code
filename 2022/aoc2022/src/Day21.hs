{-# LANGUAGE OverloadedStrings #-}
module Day21 (
    run
    , MonkeyType(..)
    , Monkey(..)
    , Expression(..)
    , parseLiteralMonkey
    , Op(..)
    , parseExpressionMonkey
    , MonkeyName(..)
    , parseMonkey
    , getMathOperationInverse
    , solveEquation
    , ItemOrder(..)
    ) where

import System.Exit (
    die
    )

import Debug.Trace

import System.FilePath (
    FilePath
    )

import qualified Data.Map as M
import Data.Maybe (
    fromJust
    )
import qualified Data.ByteString as BS
import qualified Data.ByteString as BSL
import qualified Data.ByteString as BSS (
    readFile
    )
import qualified Data.Word8 as W
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import Control.Applicative ((<|>))
import Control.Monad.State (State, get, modify, runState)
import Control.Monad.State.Lazy (execState, execStateT)

run :: FilePath -> IO ()
run fp = do
    fileContents <- BSS.readFile fp
    parseResult <- do
        case AP.parseOnly parse (BSL.fromStrict fileContents) of
            Left err -> die ("failed to parse" <> err)
            Right pr -> pure pr
    print parseResult
    partOne parseResult
    partTwo parseResult

partOne :: [Monkey] -> IO ()
partOne monkeys = do
    let monkeyMap = M.fromList $ zip (map monkeyName monkeys) monkeys
        (answer, endState) = runState (evaluateMonkeys (MonkeyName "root")) monkeyMap
    putStrLn $ "answer:" <> show answer
    putStrLn $ "answer as int:" <> show (floor <$> answer :: Maybe Int)
    --putStrLn $ "final state:" <> show endState
    putStrLn "part one finished"

-- the code now works enough to produce an answer, but it is not correct
-- 8328390935499 is too high
-- 4476 is more believable but also wrong (too low)
-- 3782852515583 babeeeeee we did it
--  part2 code is so ugly tho
partTwo :: [Monkey] -> IO ()
partTwo monkeys = do
    let monkeyMap = M.fromList $ zip (map monkeyName monkeys) (map Unprocessed monkeys)
        (answer, endState) = runState (evaluateMonkeysPart2 rootName humanName) monkeyMap
    putStrLn "part two finished"
    putStrLn $ "answer:" <> show answer <> " as int:" <> show (floor <$> answer :: Maybe Int)

rootName :: MonkeyName
rootName = MonkeyName "root"

humanName :: MonkeyName
humanName = MonkeyName "humn"

data ExpectedMonkey = SuccessfullyEvaluated Double | NeedsInference Monkey | Unprocessed Monkey
    deriving (Show)

isNeedsInference :: ExpectedMonkey -> Bool
isNeedsInference (NeedsInference _) = True
isNeedsInference _ = False

getExpectedMonkey :: ExpectedMonkey -> Maybe Monkey
getExpectedMonkey (SuccessfullyEvaluated _) = Nothing
getExpectedMonkey (NeedsInference monk) = Just monk
getExpectedMonkey (Unprocessed monk) = Just monk

type Part2MonkeyStates = M.Map MonkeyName ExpectedMonkey

evaluateMonkeysPart2 :: MonkeyName -> MonkeyName -> State Part2MonkeyStates (Maybe Double)
evaluateMonkeysPart2 startMonkeyName variableMonkeyName = do
    currState <- get
    let (preEvaluatedMonkeys, _) = evaluateSimpleMonkeys startMonkeyName currState
    case M.lookup startMonkeyName preEvaluatedMonkeys of
        Just (NeedsInference (Monkey _ (ExpressionMonkey (Expression leftMonkeyName _ rightMonkeyName)))) -> do
            let foundLeftMonkey = M.lookup leftMonkeyName preEvaluatedMonkeys
                foundRightMonkey = M.lookup rightMonkeyName preEvaluatedMonkeys
                useInferInfo = case foundLeftMonkey of
                    -- the left side was successfully evaluated, so we will infer from the right side
                    Just (SuccessfullyEvaluated _val) -> Just (rightMonkeyName, _val)
                    _ -> case foundRightMonkey of
                        -- the right side was successfully evaluated, so we will infer from the left side
                        Just (SuccessfullyEvaluated _val) -> Just (leftMonkeyName, _val)
                        _ -> Nothing
            case useInferInfo of
                Nothing -> trace "root inference setup logic failed" (pure Nothing)
                Just (needsInferredMonkeyName, inferValue) ->
                    let finalState = execState (inferMonkeys (needsInferredMonkeyName, inferValue)) preEvaluatedMonkeys in
                        case M.lookup variableMonkeyName finalState of
                            Just (SuccessfullyEvaluated answerVal) -> pure (Just answerVal)
                            _ -> trace
                                ("couldn't find the 'humn' monkey to see what value it needed to shout start monkey name:"
                                    <> show startMonkeyName
                                    <> " "
                                    <> show finalState)
                                (pure Nothing)
        _ -> trace (badMonkey startMonkeyName preEvaluatedMonkeys) (pure Nothing)
    --case M.lookup currMonkeyName currState of
    --    Nothing -> trace (badMonkey currMonkeyName currState) (pure Nothing)
    --    Just (EvaluatedTo (Monkey mName mExp)) -> undefined
    --    Just (MustEqual (Monkey mName (ExpressionMonkey $ Expression arg1 op arg2 ) mustBe) ->
    --    Just (Unprocessed monk@(Monkey mName monkType))
    --        -- don't know the value that the 'humn' monkey should carry
    --        -- so we have to return nothing to tell the calling context
    --        -- that this side needs re-evaluated with the expectation that it equal
    --        -- the other side. this isn't efficient of course
    --        | mName == variableMonkeyName -> pure Nothing
    --        | mName == startMonkeyName -> do
    --            modify $ M.adjust (const resMonkey) mName
    --        | otherwise ->
    --            case monkType of
    --                LiteralMonkey val -> pure Val
    --                Evaluated val -> pure Val
    --                ExpressionMonkey exp -> undefined
    where
    inferMonkeys :: (MonkeyName, Double) -> State Part2MonkeyStates (Maybe Double)
    inferMonkeys (currMonkeyName, inferValue) = do
        currInferState <- get
        case M.lookup currMonkeyName currInferState of
            Just (NeedsInference (Monkey mName (ExpressionMonkey (Expression left op right)))) -> do
                case pickInferenceMonkeys currInferState left right of
                    Just (inferenceSide, (evaluatedMonkey, figureOutMonkey)) -> 
                        case figureOutMonkey of
                            NeedsInference (Monkey needsInferenceName _) ->
                                case evaluatedMonkey of
                                    -- calculate the value necessary to satisfy our current infer value
                                    -- and then recurse, looking for the 'humn' monkey
                                    SuccessfullyEvaluated val ->
                                        let
                                            --inverseOpToken = getMathOperationInverse op
                                            --inverseOp = getMathOperation inverseOpToken
                                            newExpectedValue = case inferenceSide of
                                                MyLeft -> solveEquation UnknownToKnown inferValue op val
                                                    --inferValue `inverseOp` val
                                                MyRight -> solveEquation KnownToUnknown inferValue op val
                                                    --inferValue `inverseOp` val
                                        in
                                            trace ("expected:" <> show inferValue <> " existing value:" <> show val <> " op:" <> show op <> " new expected:" <> show newExpectedValue)
                                                inferMonkeys (
                                                    needsInferenceName
                                                    ,  newExpectedValue
                                                )
                                    _ -> trace "other monkey wasn't evaluated" (pure Nothing)
                            _ -> trace "figure out monkey wasn't an inference monkey" (pure Nothing)
                    Nothing -> trace "couldn't decide which monkey was evaluated and which one needs inferred" (pure Nothing)
            -- when we hit the 'humn' monkey, just return it's inferred value
            -- because that's what it needs to shout to ensure that the root monkey's side trees
            -- evaluate to the same thing
            Just (NeedsInference (Monkey mName (LiteralMonkey _))) -> do
                modify $ M.adjust (const (SuccessfullyEvaluated inferValue)) mName
                trace "found human monkey" (pure (Just inferValue))
            Just (SuccessfullyEvaluated successVal) -> pure (Just successVal)
            Just (Unprocessed m) -> pure (trace ("unprocessed monkey existed in infer monkeys, should be impossible" <> show m) Nothing)
            _ -> pure Nothing

    -- evaluate all of the monkeys that we can, leave the ones that we can't in an unfinished state
    -- then, as a second step we can infer the value that must be said by the 'humn' monkey
    evaluateSimpleMonkeys :: MonkeyName -> Part2MonkeyStates -> (Part2MonkeyStates, Maybe Double)
    evaluateSimpleMonkeys evalMonkeyName simpleMonkeyStates = do
        case M.lookup evalMonkeyName simpleMonkeyStates of
            Nothing -> trace (badMonkey evalMonkeyName simpleMonkeyStates) (simpleMonkeyStates, Nothing)
            Just (SuccessfullyEvaluated monkeyVal) -> (simpleMonkeyStates, Just monkeyVal)
            -- an unprocessed literal monkey becomes a processed monkey
            Just (Unprocessed human@(Monkey unprocessedMonkeyName (LiteralMonkey val)))
                -- if we find the human variable monkey, we need to mark it as needed inference
                -- everything that depends on it will get marked as needing inference
                | unprocessedMonkeyName == variableMonkeyName -> (M.adjust (const $ NeedsInference human) unprocessedMonkeyName simpleMonkeyStates, Nothing)
                | otherwise -> (M.adjust (const $ SuccessfullyEvaluated (fromIntegral val :: Double)) evalMonkeyName simpleMonkeyStates, Just (fromIntegral val :: Double))
            Just (Unprocessed unprocessedMonkey@(Monkey _ (ExpressionMonkey (Expression leftMonkey op rightMonkey)))) ->
                let (newStateAfterLeft, evalLeftMonkey) = evaluateSimpleMonkeys leftMonkey simpleMonkeyStates
                    (newStateAfterRight, evalRightMonkey) = evaluateSimpleMonkeys rightMonkey newStateAfterLeft
                    mathOp = getMathOperation op
                in
                if isJust evalLeftMonkey && isJust evalRightMonkey
                then let result = fromJust evalLeftMonkey `mathOp` fromJust evalRightMonkey in
                            (M.adjust (const $ SuccessfullyEvaluated result) evalMonkeyName newStateAfterRight, Just result)
                else (M.adjust (const $ NeedsInference unprocessedMonkey) evalMonkeyName newStateAfterRight, Nothing)
            Just (NeedsInference _) -> (simpleMonkeyStates, Nothing)
            Just (Unprocessed (Monkey _ (Evaluated evaluatedValue))) -> (simpleMonkeyStates, Just evaluatedValue)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

data Dir = MyLeft | MyRight deriving (Show)

-- returs (monkey that was successfully evaluated, monkey that needs it's value inferred)
pickInferenceMonkeys :: Part2MonkeyStates -> MonkeyName -> MonkeyName -> Maybe (Dir, (ExpectedMonkey, ExpectedMonkey))
pickInferenceMonkeys monkeyStates leftMonkeyName rightMonkeyName = do
    leftMonkey <- M.lookup leftMonkeyName monkeyStates
    rightMonkey <- M.lookup rightMonkeyName monkeyStates
    let dir = if isNeedsInference leftMonkey then MyLeft else MyRight
        figureOutMonkey = if isNeedsInference leftMonkey then leftMonkey else rightMonkey
        otherMonkey = if isNeedsInference leftMonkey then rightMonkey else leftMonkey
    pure (dir, (otherMonkey, figureOutMonkey))

type MonkeyStates = M.Map MonkeyName Monkey

badMonkey :: (Show a, Show b) => a -> b -> String
badMonkey rawMonkeyName currMap = "monkey:" <> show rawMonkeyName <> " did not exist in map:" <> show currMap

evaluateMonkeys :: MonkeyName -> State MonkeyStates (Maybe Double)
evaluateMonkeys thisMonkeyName@(MonkeyName rawMonkeyName) = do
    monkeyMap <- get
    evalResult <- case M.lookup thisMonkeyName monkeyMap of
        Nothing -> trace (badMonkey rawMonkeyName monkeyMap) (pure Nothing)
        Just (Monkey _ (Evaluated val)) -> pure (Just val)
        Just (Monkey _ (LiteralMonkey val)) -> pure (Just (fromIntegral val :: Double))
        Just (Monkey _ (ExpressionMonkey (Expression left op right))) -> do
            evalLeftMonkey <- evaluateMonkeys left
            evalRightMonkey <- evaluateMonkeys right
            let mathOperation = getMathOperation op
                result = mathOperation <$> evalLeftMonkey <*> evalRightMonkey
            case result of
                Nothing -> pure Nothing
                Just res -> do
                    let resMonkey = Monkey thisMonkeyName (Evaluated res)
                    modify $ M.adjust (const resMonkey) thisMonkeyName
                    pure result
    pure evalResult

getMathOperation :: (Fractional a) => Op -> (a -> a -> a)
getMathOperation op =
    case op of
        Add -> (+)
        Sub -> (-)
        Mult -> (*)
        Div -> (/)

getMathOperationInverse :: Op -> Op
getMathOperationInverse op =
    case op of
        Add -> Sub
        Sub -> Add
        Mult -> Div
        Div -> Mult

newtype MonkeyName = MonkeyName BSL.ByteString
    deriving (Show, Eq, Ord)

data Op = Add | Sub | Mult | Div deriving (Show, Eq)

-- | it looks like Monkeys that have expressions can't do math with literals, on the result of evaluating other monkeys
data Expression = Expression {
    expArg1 :: MonkeyName
    , expOp :: Op
    , expArg2 :: MonkeyName
    }
    deriving (Show, Eq)

data MonkeyType = Evaluated Double
    | LiteralMonkey Int
    | ExpressionMonkey Expression
    deriving (Show, Eq)

-- n is the string type for the monkey name
-- well, I wanted it to be parameterized but I'm not sure how I can do this and
-- derive Show
--data Monkey n = (Eq n) => Monkey n MonkeyType
--deriving instance Show (Monkey BS.ByteString)
data Monkey = Monkey MonkeyName MonkeyType
    deriving (Show, Eq)

monkeyName :: Monkey -> MonkeyName
monkeyName (Monkey name _) = name

data ItemOrder = UnknownToKnown | KnownToUnknown

-- |which side of the operator known val is on -> result -> operation -> known operand
-- this function takes a binary operation, one operand, and the side of the operator
-- that the unknown value is on, and then solves the equation to find the unknown operand
solveEquation :: ItemOrder -> Double -> Op -> Double -> Double
solveEquation itemOrder result op knownArg =
    case itemOrder of
        KnownToUnknown ->
            -- result = knownArg op <unknownVal>
            case op of
                -- result - knownArg = <unk>
                Add -> result - knownArg
                -- result + <unk> = knownArg
                -- <unk> = knownArg - result
                Sub -> knownArg - result
                -- <unk> = result / knownArg
                Mult -> result / knownArg
                -- result * <unk> = knownArg
                -- <unk> = knownArg / result
                Div -> knownArg / result
        UnknownToKnown ->
            -- result = <unknownVal> op knownArg
            case op of
                -- result - knownArg = <unk>
                Add -> result - knownArg
                -- result + knownArg = <unk>
                Sub -> result + knownArg
                -- result / knownArg = <unk>
                Mult -> result / knownArg
                -- result = <unk> op knownArg
                -- result * knownArg = unk
                -- <unk> = knownArg / result
                Div -> knownArg * result



parse :: AP.Parser [Monkey]
parse = AP.sepBy' parseMonkey APC.endOfLine

parseMonkey :: AP.Parser Monkey
parseMonkey = do
    parsedMonkeyName <- (MonkeyName <$> AP.takeTill (== W._colon)) AP.<?> "parse monkey name"
    _ <- AP.word8 W._colon
    _ <- AP.word8 W._space
    monkeyValue <- (parseLiteralMonkey <|> parseExpressionMonkey) AP.<?> "parse monkey value"
    pure $ Monkey parsedMonkeyName monkeyValue
    AP.<?> "parseMonkey"

parseLiteralMonkey :: AP.Parser MonkeyType
parseLiteralMonkey = LiteralMonkey <$> APC.decimal

parseExpressionMonkey :: AP.Parser MonkeyType
parseExpressionMonkey = ExpressionMonkey <$> parseExpression

skipSpace :: AP.Parser ()
skipSpace = AP.skip (== W._space)

parseExpression :: AP.Parser Expression
parseExpression = Expression <$>
    (parseMonkeyNameInExp <* skipSpace)
    <*> (parseOp <* skipSpace)
    <*> parseMonkeyNameInExp
    AP.<?> "parseExpression"

parseMonkeyNameInExp :: AP.Parser MonkeyName
parseMonkeyNameInExp = MonkeyName
    <$> AP.takeWhile (AP.inClass "a-z")
    AP.<?> "parseMonkeyNameInExp"

parseOp :: AP.Parser Op
parseOp =  AP.choice [AP.word8 W._plus >> pure Add
    , AP.word8 W._hyphen >> pure Sub
    , AP.word8 W._asterisk >> pure Mult
    , AP.word8 W._slash >> pure Div
    ] APC.<?> "parse operator" 
