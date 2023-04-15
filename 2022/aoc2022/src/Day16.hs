module Day16 where

import qualified Data.Map as M

import Control.Monad.State

import Debug.Trace

import Data.Maybe

import Text.Parsec as P hiding (State)
import qualified Text.Parsec as P (State)

import qualified Data.Foldable as F

import Data.List (
    maximumBy
    )

import Parsing (
    digits
    , plainWhitespace
    , plural
    )
import Data.Function (on)

data Valve = Valve String Int [String] Bool
    deriving (Show, Ord, Eq)

valveIsOn :: Valve -> Bool
valveIsOn (Valve _ _ _ isOn) = isOn

valveName :: Valve -> String
valveName (Valve nm _ _ _) = nm

valveP :: Valve -> Int
valveP (Valve _ pressure _ _) = pressure

turnOn :: Valve -> Valve
turnOn (Valve vNm vP vNhbrs _) = Valve vNm vP vNhbrs True

data ValveState = ValveState { vState::Valves, currVName :: String, rTime :: Int }
    deriving (Show, Ord, Eq)


data Action = Move String String | Activate String deriving (Show)

type Valves = M.Map String Valve

type MemoSolutions = M.Map ValveState (Int, [Action])

--part 1 answer 2119
run :: FilePath -> IO ()
run filepath = do
    print $ "day16, file path:" ++ filepath
    filecontent <- readFile filepath
    case P.runParser Day16.parse () filepath filecontent of
        Left e -> do
            print "parsing error"
            print e
        Right parseResult -> do
            print $ "parse result:" ++ show parseResult
            let startValve = (valveName . head) parseResult
            let simDuration = 30
            print $ "starting at valve:" ++ startValve ++ " with a simulation duration of:" ++ show simDuration
            let valvesAsMap = M.fromList [(valveName v, v) | v <- parseResult]
                startState = ValveState {
                    vState=valvesAsMap
                    , currVName=startValve
                    , rTime=simDuration
                    }
            let ((totalPressureReleased, bestActions), finalMemoState) = runState (solve startState) M.empty
            print $ (show . length) bestActions
            mapM_ print bestActions
            --print $ "final valve state:" ++ show (vState finalValveState)
            --print $ "final valve state pressure per minute:" ++ show (evaluateValves finalValveState)
            print $ "total pressure released:" ++ show totalPressureReleased
            --let solutionPressurePerMinute = evaluateValves (foldl applyAction bestActions)
            --print $ "pressure per minute of final solution:" ++ show (evaluateValves
            --
            --let ((answerPressure, answerValves, answerActions), answerFinalState) = runState (solve valvesAsMap ((valveName . head) parseResult) 3) M.empty
            --print $ "# moves made:" ++ show (length answerActions)
            --mapM_ print answerActions

-- finds the optimal list of actions to be taken at each minute
solve :: ValveState -> State MemoSolutions (Int, [Action])
solve vs
    | rTime vs == 0 = return (0, [])
    | null (vState vs) = return (0, [])
    | otherwise = do
        memoedSolutions <- get
        case M.lookup vs memoedSolutions of
            --Just (totalPressureReleased, actions) -> trace "cache hit!" (return (totalPressureReleased, actions))
            Just (totalPressureReleased, actions) -> return (totalPressureReleased, actions)
            Nothing -> do
                let possibleActions = generateActions vs
                subSolutions <- mapM (solve . applyAction vs) possibleActions
                let subSolutionsWithAction = zip possibleActions subSolutions
                    --evaluatedSubSolutions = map (\(newAction, (newVs, actionsTaken)) -> (newVs, newAction, actionsTaken)) subSolutionsWithAction
                    bestSolution@(actionToTake, (subPressureReleased, answerActions)) = maximumBy (compare `on` (\(_, (subTreeReleasedPressure, _)) -> subTreeReleasedPressure)) subSolutionsWithAction
                    allActions = actionToTake:answerActions
                    pressureReleased = subPressureReleased + evaluateValves vs
                --this makes easier to see what is happening
                --modify (\stateWithUpdatedMemos -> M.insert answerState allActions stateWithUpdatedMemos)
                modify $ M.insert vs (pressureReleased, allActions)
                return (pressureReleased, allActions)

--calcReleasedPressure :: ValveState -> [Action] -> Int
--calcReleasedPressure vs actions
--    | rTime vs == 0 = evaluateValves vs
--    | otherwise =
--        let pressureReleasedThisMinute = evaluateValves vs
--            nextValveState = if null actions then vs { rTime=rTime vs - 1 } else applyAction vs (head actions)
--        in
--            pressureReleasedThisMinute + calcReleasedPressure nextValveState (tail actions)

--solve :: Valves -> String -> Int -> State Solutions (Int, Valves, [Action])
--solve valves currValve time
--    | null valves = pure (0, valves, [])
--    | time == 0 = pure (0, valves, [])
--    | otherwise = do
--        let (Valve vName _ _ isOn) = fromJust (M.lookup currValve valves)
--        cachedSolutions <- get
--        case M.lookup (vName, time, isOn) cachedSolutions of
--            Just cachedAnswer -> return cachedAnswer
--            Nothing -> do
--                let actions = generateActions valves currValve
--                    subTrees = map (applyAction valves time) actions
--                subSolutions <- mapM (\(newTime, newValves, newCurrValve) -> solve newValves newCurrValve newTime) subTrees
--                let bestSubSolution@(_, newValves, actionsTaken) = F.maximumBy (compare `on` (\(p, _, _) -> p)) subSolutions
--                    thisSolution = (evaluateValves newValves, newValves, actionsTaken)
--                -- a lambda was more easy for me to understand but the compiler complained that I needed to 'avoid lambda' (\oldState -> M.insert ... newState)
--                modify $ M.insert (vName, time, isOn) thisSolution
--                return thisSolution
--
                --    subSolutions = mapM (\(newTime, newValves, newLoc) -> solve newValves newLoc newTime) subTrees
                --    bestSolution = F.maximumBy (compare `on` fmap  fst) subSolutions
                --put (M.insert (vName, time, isOn) bestSolution cachedSolutions)
                --return bestSolution

-- calculate the pressure released by the list of valves per minute
evaluateValves :: ValveState ->  Int
evaluateValves valves = sum (M.map valveP onValves)
    where
        onValves = M.filter valveIsOn (vState valves)

-- returns (time after action, valve list after action, current valve after action)
applyAction :: ValveState -> Action -> ValveState
applyAction valves act =
    --case trace ("applied:" ++ show act) act of
    case act of
        Move _ to -> valves { rTime=newTime, currVName=to }
        Activate targetValveName -> let targetValve = fromJust (M.lookup targetValveName (vState valves)) in
            valves { rTime=newTime, vState=M.insert targetValveName (turnOn targetValve) (vState valves) }
        where
            --newTime = trace (show $ rTime valves - 1) (rTime valves - 1)
            newTime = rTime valves - 1

activateValve :: Valve -> Valve
activateValve (Valve nm pressure nhbrs _) = Valve nm pressure nhbrs True

generateActions :: ValveState -> [Action]
generateActions vs
    | null (vState vs) = []
    | otherwise = case M.lookup (currVName vs) (vState vs) of
        Nothing -> []
        Just (Valve name pressure neighbors isOn) -> movements ++ (if isOn || pressure == 0 then [] else [Activate name])
            where
                movements = makeMovements name neighbors

--generateActions :: Valves -> String -> [Action]
--generateActions valves currentValve
--    | null valves = []
--    | otherwise = case M.lookup currentValve valves of
--        Nothing -> []
--        Just (Valve name _ neighbors isOn) -> movements ++ (if isOn then [] else [Activate name])
--            where
--                movements = makeMovements name neighbors

makeMovements :: String -> [String] -> [Action]
makeMovements thisValve = map (Move thisValve)

valveStatement :: P.Parsec String () Valve
valveStatement = do
    _ <- string "Valve" <* space
    newValveName <- many letter <* space
    _ <- string "has flow rate="
    flowrate <- digits
    _ <- char ';' 
    _ <- plainWhitespace
    _ <- string "tunnel" <* plural
    _ <- plainWhitespace
    _ <- string "lead" <* plural
    _ <- plainWhitespace
    _ <- string "to "
    _ <- string "valve" <* plural
    _ <- plainWhitespace
    tunnels <- many letter `sepBy1` string ", "
    return $ Valve newValveName flowrate tunnels False

parse :: P.Parsec String () [Valve]
parse = many (valveStatement <* endOfLine) <* eof
