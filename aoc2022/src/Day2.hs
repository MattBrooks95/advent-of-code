module Day2 where

import Prelude hiding (
    read
    )

import Data.Maybe (
    fromJust
    , catMaybes
    )

data Player = Me | Enemy deriving (Show)
data Choice = Rock Player | Paper Player | Scissors Player deriving (Show)

data Game = Game Choice Choice deriving (Show)
--opponentChoice (Game x _) = x
--myChoice (Game _ x) = x

data GameResult = Win | Loss | Draw

--returns the choice y that beats choice x
beats :: Choice -> Choice
beats x = case x of
    Rock _ -> Paper Me
    Paper _ -> Scissors Me
    Scissors _ -> Rock Me

draws :: Choice -> Choice
draws x = x

loses :: Choice -> Choice
loses x = case x of
    Rock _ -> Scissors Me
    Paper _ -> Rock Me
    Scissors _ -> Paper Me

reinterpretGame :: Game -> Game
reinterpretGame (Game oppChoice myChoice) = Game oppChoice (choiceToRigGame reinterpretedChoice oppChoice)
    where
        reinterpretedChoice = reinterpretChoice myChoice
--(\(Game oppChoice myChoice) ->
--(Game oppChoice (choiceToRigGame (reinterpret myChoice) oppChoice) games)

reinterpretChoice :: Choice -> GameResult
reinterpretChoice choice = case choice of
    Rock _ -> Loss
    Paper _ -> Draw
    Scissors _ -> Win

-- returns the choice that needs to be made to get the desired game result
-- desired result, opponent's choice -> choice needed for desired result
choiceToRigGame :: GameResult -> Choice -> Choice
choiceToRigGame result oppChoice = case result of
    Win -> beats oppChoice
    Loss -> loses oppChoice
    Draw -> draws oppChoice

gameResult :: Game -> GameResult
gameResult game = case game of
    Game (Rock _) (Paper _) -> Win
    Game (Paper _) (Rock _) -> Loss
    Game (Scissors _) (Paper _) -> Loss
    Game (Paper _) (Scissors _) -> Win
    Game (Rock _) (Scissors _) -> Loss
    Game (Scissors _) (Rock _) -> Win
    _ -> Draw

scoreResult :: GameResult -> Int
scoreResult Win = 6
scoreResult Loss = 0
scoreResult Draw = 3

readMove :: Char -> Maybe Choice
readMove x = case x of
    'A' -> Just $ Rock Enemy
    'B' -> Just $ Paper Enemy
    'C' -> Just $ Scissors Enemy
    'X' -> Just $ Rock Me
    'Y' -> Just $ Paper Me
    'Z' -> Just $ Scissors Me
    _ -> Nothing

scoreMove :: Choice -> Int
scoreMove mv = case mv of
    Rock _ -> 1
    Paper _ -> 2
    Scissors _ -> 3

score :: Game -> Int
score game@(Game _ myChoice) = winPoints + movePoints
    where
        movePoints = scoreMove myChoice
        winPoints = scoreResult $ gameResult game

-- TODO rewrite this with do notation instead of nesting cases
read :: String -> Maybe Game
read [] = Nothing
read line = 
    case opponentMove of
        Nothing -> Nothing
        Just _ -> case myMove of
            Nothing -> Nothing
            Just _ -> Just (Game (fromJust opponentMove) (fromJust myMove))
    where
        opponentMove = readMove $ head line
        myMove = readMove $ last line

dayTwo :: [String] -> IO ()
dayTwo inputLines = do
    --print "TODO day two"
    --print inputLines
    let potentialGames = map Day2.read inputLines
    --print potentialGames
    let games = catMaybes potentialGames
    let gameScores = map score games
    --print gameScores
    print $ sum gameScores
    -- below is part 2
    let riggedGames = map reinterpretGame games
    --print $ take 5 games
    --print $ take 5 riggedGames
    --I got my star but I'm not proud of how I handled this
    --I wish I could abstract away some of the cases statements used to
    --determine which choice beats, loses or draws to which choice
    print $ sum (map score riggedGames)

