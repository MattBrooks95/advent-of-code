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
    , find
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

getParent :: Node -> Node
getParent me@(DirNode (Dir _ _ _ parent)) = fromMaybe me parent
getParent me@(FileNode _) = me

data CDTarget = GoParent | GoDir String | GoRoot deriving (Show)
data Command = CD CDTarget | LS [String]  deriving (Show)

instance (Show Node) where
    show (FileNode (File name size)) = "(file:" ++ name ++ " size:" ++ show size
    show (DirNode (Dir name children size parent)) =
        "(dir:" ++ name
        ++ " size:" ++ show size
        ++ " parent:" ++ maybe "nil" getName parent
        ++ " children:(" ++ intercalate "," (map show children) ++ ")"

getName :: Node -> String
getName (FileNode (File name _)) = name
getName (DirNode (Dir name _ _ _)) = name

getChildren :: Node -> [Node]
getChildren (FileNode (File _ _)) = []
getChildren (DirNode (Dir _ children _ _)) = children

-- traverse from a node upwards to the root directory
toTop :: Node -> Node
toTop node@(DirNode (Dir _ _ _ parent)) = maybe node toTop parent
-- I really hate that I can't specify in the type signature that this HAS
-- to be a DirNode. I should ask for help in this regard
toTop node@(FileNode (File _ _)) = trace "toTop was given a file node..." node
--case parent of
--    Nothing -> node
--    Just parent -> toTop parent
--data Dir = Dir String [Node] (Maybe Int) (Maybe Node)

run :: [String] -> IO ()
run inputLines = do
    print "day7"
    let commands = parseLines inputLines
    print commands
    print $ toTop $ evalCommands commands (DirNode (Dir "/" [] Nothing Nothing))

evalCommands :: [Command] -> Node -> Node
evalCommands [] fileTree = toTop fileTree
evalCommands (c:cs) fileTree = case c of
    CD GoRoot -> evalCommands cs (toTop fileTree)
    CD GoParent -> evalCommands cs (getParent fileTree)
    CD (GoDir str) -> evalCommands cs (fromMaybe fileTree (find ((== str) . getName) (getChildren fileTree)))
    LS strings -> evalCommands cs (
        case fileTree of
            FileNode _ -> trace "LS command in a file node..." fileTree
            me@(DirNode (Dir name children size parent)) -> DirNode (Dir name (parseLs parent strings) size (Just me))
        )

--data Dir = Dir String [Node] (Maybe Int) (Maybe Node)
--data CDTarget = GoParent | GoDir String | GoRoot deriving (Show)
--data Command = CD CDTarget | LS [String]  deriving (Show)

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

parseLs :: Maybe Node -> [String] -> [Node]
parseLs parent inputStrings = map (parseNode parent) inputStrings

parseNode :: Maybe Node -> String -> Node
parseNode parent input = if length input >= 4 && take 3 input == "dir"
    then DirNode (Dir (drop 4 input) [] Nothing parent)
    else FileNode (File (matches !! 2) (read (matches !! 1) :: Int))
        where
            matches = getAllTextSubmatches (input =~ "([0-9]+) ([a-z.]+)") :: [String]

