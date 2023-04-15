module Day16 where

import qualified Data.Map as M

import Control.Monad.State

import Data.Maybe

import Text.Parsec as P hiding (State)
import qualified Text.Parsec as P (State)

import qualified Data.Foldable as F

import Parsing (
    digits
    , plainWhitespace
    , plural
    )
import Data.Function (on)

data Valve = Valve String Int [String] Bool
    deriving (Show)

valveName :: Valve -> String
valveName (Valve nm _ _ _) = nm

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
            let valvesAsMap = M.fromList (map (\v -> (valveName v, v)) parseResult)
            let (_, answer) = runState (solve valvesAsMap ((valveName . head) parseResult) 30) M.empty
            print $ "# moves made:" ++ show (length answer)

data Action = Move String String | Activate String deriving (Show)

type Valves = M.Map String Valve

type Solutions = M.Map (String, Int, Bool) (Int, [Action])

solve :: Valves -> String -> Int -> State Solutions (Int, [Action])
solve valves currValve time
    | null valves = pure (0, [])
    | time == 0 = pure (0, [])
    | otherwise = do
        let (Valve vName _ _ isOn) = fromJust (M.lookup currValve valves)
        cachedSolutions <- get
        case M.lookup (vName, time, isOn) cachedSolutions of
            Just cachedAnswer -> return cachedAnswer
            Nothing -> do
                let actions = generateActions valves currValve
                    subTrees = map (applyAction valves time) actions
                subSolutions <- mapM (\(newTime, newValves, newCurrValve) -> solve newValves newCurrValve newTime) subTrees
                let bestSolution = F.maximumBy (compare `on` fst) subSolutions
                -- a lambda was more easy for me to understand but the compiler complained that I needed to 'avoid lambda' (\oldState -> M.insert ... newState)
                modify $ M.insert (vName, time, isOn) bestSolution
                return bestSolution

                --    subSolutions = mapM (\(newTime, newValves, newLoc) -> solve newValves newLoc newTime) subTrees
                --    bestSolution = F.maximumBy (compare `on` fmap  fst) subSolutions
                --put (M.insert (vName, time, isOn) bestSolution cachedSolutions)
                --return bestSolution

-- returns (time after action, valve list after action, current valve after action)
applyAction :: Valves -> Int -> Action -> (Int, Valves, String)
applyAction valves time act = case act of
    Move _ to -> (time - 1, valves, to)
    Activate valve -> (time - 1, M.update (Just . activateValve) valve valves, valve)

activateValve :: Valve -> Valve
activateValve (Valve nm pressure nhbrs _) = Valve nm pressure nhbrs True

generateActions :: Valves -> String -> [Action]
generateActions valves currentValve
    | null valves = []
    | otherwise = case M.lookup currentValve valves of
        Nothing -> []
        Just (Valve name _ neighbors isOn) -> movements ++ (if isOn then [] else [Activate name])
            where
                movements = makeMovements name neighbors

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
