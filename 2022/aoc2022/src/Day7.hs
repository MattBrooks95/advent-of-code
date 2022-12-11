module Day7 where

import Prelude hiding (
    lines
    )

import Debug.Trace

import Data.Maybe (
    fromMaybe
    )

import Data.List (
    intercalate
    )

import Text.Regex.TDFA

-- a node in the tree is a leaf (file) or a node (dir)
data Node = FileNode File | DirNode Dir
-- a file is a name and a size
data File = File String Int
-- a dir is a name, a list of child nodes(files or subdirs) a size(may or may not have been calculated alread)
-- and a reference to the parent node (which does not exist for the root directory '/'
-- the parent must be a DirNode, how do I convey that with the type system?
-- (Maybe DirNode) doesn't compile
-- it looks like it is a limitation of the type system, and the LSP is recommending
-- the DataKinds language extension: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html
-- even with the extension, it becomes an error, something about not being able to use it
-- because it's in the 'same recursive group'
data Dir = Dir String [Node] (Maybe Int) (Maybe Node)

data CDTarget = GoParent | GoDir String | GoRoot deriving (Show)
data Command = CD CDTarget | LS [String]  deriving (Show)

instance (Show Node) where
    show (FileNode (File name size)) = "(file:" ++ name ++ " size:" ++ show size
    show (DirNode (Dir name children size parent)) =
        "(dir:" ++ name
        ++ " size:" ++ show size
        ++ " parent:" ++ maybe "nil" getName parent
        ++ " children:" ++ intercalate "," (map getName children)

getName :: Node -> String
getName (FileNode (File name _)) = name
getName (DirNode (Dir name _ _ _)) = name

run :: [String] -> IO ()
run inputLines = do
    print "day7"
    let commands = parseLines inputLines
    print commands

--evalCommands :: [Command]

buildFileSystem :: [String] -> Node
buildFileSystem inputs = DirNode $ Dir "/" [] Nothing Nothing

parseLines :: [String] -> [Command]
parseLines = parseLines' []

parseLines' :: [Command] -> [String] -> [Command]
parseLines' acc [] = reverse acc
parseLines' acc (l:ls) = parseLines' (command:acc) remainingInput
    where
        (command, remainingInput) = parseCommand l ls

parseCommand :: String -> [String] -> (Command, [String])
parseCommand input nextInputs = if length input >= 4 && take 4 input == "$ ls"
    then (LS commandLines, remainingInput)
    else case cdTargetMatch !! 1 of
        ".." -> (CD GoParent, nextInputs)
        "/" -> (CD GoRoot, nextInputs)
        _ -> (CD (GoDir (cdTargetMatch !! 1)), nextInputs)
    where
        cdTargetMatch = getAllTextSubmatches (input =~ "\\$ cd (.*)") :: [String]
        (commandLines, remainingInput) = parseCommandOutputs nextInputs []

-- read lines for a command until end of list
-- or the beginning of another command
-- returns the strings that represent the outputs of the parsed command,
-- and the remaining input
parseCommandOutputs :: [String] -> [String] -> ([String], [String])
parseCommandOutputs [] linesForCommand = (reverse linesForCommand, [])
parseCommandOutputs input@(x:xs) linesForCommand = if isCommand x
    then (linesForCommand, input)
    else parseCommandOutputs xs (x:linesForCommand)

isCommand :: String -> Bool
isCommand input = not (null input) && head input == '$'
