module Day2 where

import Prelude hiding (
    read
    )

import Data.Maybe (
    fromJust
    )

data Player = Me | Enemy deriving (Show)
data Choice = Rock Player | Paper Player | Scissors Player deriving (Show)

data Game = Game Choice Choice deriving (Show)
opponentChoice (Game x _) = x
myChoice (Game _ x) = x

data GameResult = Win | Loss | Draw

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
score game = winPoints + movePoints
    where
        movePoints = scoreMove $ myChoice game
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

