--{-# LANGUAGE LambdaCase #-}
---- {-# LANGUAGE RecordWildCards #-}
module Day7 (
    run
    ) where

import Prelude hiding (
    lines
    )

import Data.List (
    find
    , intercalate
    )

import Debug.Trace

import Data.Either

import Text.Regex.TDFA

run :: [String] -> IO ()
run lines = do
    mapM_ putStrLn lines
    print $ runSim (SimState { simTree=Node (Dir "/" []) [], currDirPath=["/"], inputLines=lines })

data SimState = SimState {
    simTree :: Tree Dir
    , currDirPath :: [String]
    , inputLines :: [String]
    }

instance Show SimState where
    show (SimState tree path lines) =
        "currDirPath:"
        ++ show path
        ++ " inputLines:" ++ intercalate "," lines
        ++ "tree:" ++ showSimTree tree 0

showSimTree :: Tree Dir -> Int -> String
showSimTree Null _ = ""
showSimTree (Node (Dir name files) children) depth =
    "\n" ++ replicate depth '\t' ++ "dir:"
    ++ name ++ " files:" ++ concatMap show files
    ++ concatMap (\x -> showSimTree x (depth + 1)) children

data Tree a = Node a [Tree a] | Null deriving (Show)

data File = File String Int deriving (Show)
data Dir = Dir String [File] deriving (Show)
dirName (Dir name _) = name

-- I had no idea how to update an immutable tree
-- referenced this: https://stackoverflow.com/questions/52787042/update-a-tree-based-on-index-haskell
-- I guess the gist of it is you recursively call the constructor, and then when you've found the node that
-- needs replaced you return the new node instead of the old one
runSim :: SimState -> SimState
runSim ss = do
    --let currDir = findNode (simTree ss) (currDirPath ss)
    let inputs = inputLines ss
    case inputs of
        (x:xs) -> let (command, remainingInputs) = parseCommand x xs in
            case command of
                CdRoot -> runSim (ss { currDirPath=take 1 (currDirPath ss), inputLines=remainingInputs })
                CdUp -> runSim (ss { currDirPath=init (currDirPath ss), inputLines=remainingInputs })
                CdDown str -> runSim (ss { currDirPath=currDirPath ss ++ [str], inputLines=remainingInputs } )
                Ls lsInputs -> runSim (ss {
                    inputLines=remainingInputs
                    , simTree=updateSimTree (simTree ss) newSubTree (currDirPath ss)
                    --, simTree=updateSimTree (simTree ss) (trace ("new tree:" ++ show newSubTree) newSubTree) (currDirPath ss)
                    })
                    where newSubTree = makeSubTree lsInputs (last $ currDirPath ss)
        [] -> ss

data Command = CdRoot | CdUp | CdDown String | Ls [String]

makeSubTree :: [String] -> String -> Tree Dir
makeSubTree [] rootDirName = Node (Dir rootDirName []) []
makeSubTree inputs rootDirName = let newNodes = map makeNewNode inputs in
    Node (Dir rootDirName (rights newNodes)) (lefts newNodes)

makeNewNode :: String -> Either (Tree Dir) File
makeNewNode line =
    if length line > 3 && take 3 line == "dir"
    then Left (Node (Dir (drop 4 line) []) [])
    else Right (File (matches !! 2) (read (matches !! 1) :: Int))
    where
        matches = getAllTextSubmatches (line =~ "([0-9]+) ([a-z.]+)") :: [String]

updateSimTree :: Tree Dir -> Tree Dir -> [String] -> Tree Dir 
updateSimTree Null _ _ = Null
updateSimTree wholeTree _ [] = trace (show "returned whole tree, empty dirpath") wholeTree
updateSimTree wholeTree@(Node (Dir name _) _) newSubTree [dp] =
    if name == dp
    then newSubTree
    else wholeTree
--updateSimTree wholeTree@(Node (Dir name _) _) newSubTree [dp] = trace (show "returned new sub tree, dirpath:" ++ dp ++ " prev:" ++ name) newSubTree
updateSimTree wholeTree@(Node (Dir name files) children) newSubTree (x:xs) =
    if name == x
    then Node (Dir name files) (map (\oldChild -> updateSimTree oldChild newSubTree xs) children)
    else wholeTree
    -- then trace (show $ "name:" ++ name ++ " matched x:" ++ x) (Node (Dir name files) (map (\oldChild -> updateSimTree oldChild newSubTree xs) children))
    -- else trace (show "name:" ++ " did not match dirname:" ++ x ++ "returning whole tree") wholeTree

--updateSimTree wholeTree@(Node (Dir name) children) newSubTree insertLocation = if 

parseCommand :: String -> [String] -> (Command, [String])
parseCommand input nextInputs = if length input >= 4 && take 4 input == "$ ls"
    then (Ls commandLines, remainingInput)
    else case cdTargetMatch !! 1 of
        ".." -> (CdUp, nextInputs)
        "/" -> (CdRoot, nextInputs)
        _ -> (CdDown (cdTargetMatch !! 1), nextInputs)
    where
        cdTargetMatch = getAllTextSubmatches (input =~ "\\$ cd (.*)") :: [String]
        (commandLines, remainingInput) = getInputsTillCommand ([], nextInputs)

getInputsTillCommand :: ([String], [String]) -> ([String], [String])
getInputsTillCommand result@(_, []) = result
getInputsTillCommand (currCommandInputs, x:xs) =
    if not (null x) && head x == '$'
    then (currCommandInputs, x:xs)
    else getInputsTillCommand (currCommandInputs ++ [x], xs)

--findNode :: Tree Dir File -> [String] -> Maybe (Tree Dir File)
--findNode tree [] = Just tree
--findNode tree (dirName:dirNames) = case tree of
--    (Node (Dir name) children) ->
--        if name == dirName
--        then Nothing
--        else do
--            let nextDir = find (\case { (Node (Dir nm) _)-> nm == dirName; Leaf _ -> False; }) children
--            case nextDir of
--                Just next -> findNode next dirNames
--                Nothing -> Nothing
--    Leaf _ -> Nothing

-- I need to make it so that files aren't nodes
-- they actually aren't, there just a list of files held by a dir node
-- but because the type exists my pattern matches are getting complicated
--sumNode :: Tree Dir File -> (String, Int)
--sumNode (Leaf (File _ _)) = ("", 0)
--sumNode (Node (Dir name children)) = ("

--import Debug.Trace
--
--import Data.Maybe (
--    fromMaybe
--    )
--
--import Data.List (
--    intercalate
--    , find
--    )
--
--import Text.Regex.TDFA
--
---- a file is a name and a size
--data File = File String Int deriving (Show)
---- a dir is a name, a list of child nodes(files or subdirs) a size(may or may not have been calculated alread)
---- and a reference to the parent node (which does not exist for the root directory '/'
---- the parent must be a DirNode, how do I convey that with the type system?
---- (Maybe DirNode) doesn't compile
---- it looks like it is a limitation of the type system, and the LSP is recommending
---- the DataKinds language extension: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html
---- even with the extension, it becomes an error, something about not being able to use it
---- because it's in the 'same recursive group'
----data Dir = Dir String [Int] [File] (Maybe Int) Int Int
--
--data Dir = Dir { dirName::String, dirFiles::[File] } deriving (Show)
--
--data DirTree = DirTree (Dir, [DirTree]) | EmptyDirTree
--
--data CDTarget = GoParent | GoDir String | GoRoot deriving (Show)
--data Command = CD CDTarget | LS [String]  deriving (Show)
--
--data CommandResult = NewSubTree DirTree | NewFiles [File]
--
----instance (Show Dir) where
----    --show (FileNode (File name size)) = "(file:" ++ name ++ " size:" ++ show size
----    show (Dir name children size files parent dirId) =
----        "(dir:" ++ name
----        ++ " id:" ++ show dirId
----        ++ " size:" ++ show size
----        ++ "files:" ++ show files
----        ++ " parent id:" ++ show parent
----        ++ " children:(" ++ intercalate "," (map show children) ++ ")"
--
----getDirName :: Dir -> String
----getDirName (Dir name _ _ _ _ _) = name
----
----getParentId :: Dir -> Int
----getParentId (Dir _ _ _ _ dirId _) = dirId
----
----getFileName :: File -> String
----getFileName (File name _) = name
----
----getChildren :: Dir -> [Int]
----getChildren (Dir _ childrenIds _ _ _ _) = childrenIds
--
--run :: [String] -> IO ()
--run inputLines = do
--    print "day7"
--    let commands = parseLines inputLines
--    print commands
--    --print $ evalCommands commands [Dir "/" [] [] Nothing 0 1]
--    print $ evalCommands commands 
--
--evalCommands :: [Command] -> DirTree -> (DirTree, Bool, [Command])
--evalCommands [] dirTree = (dirTree, False, [])
--evalCommands (c:cs) wholeTree@(DirTree (me@Dir { }, children)) =
--        case commandResult of
--            ((DirTree (Dir {}, newChildren@(_))), True, commands) -> (DirTree (me, children ++ newChildren), True, commands)
--    where 
--        commandResult = case c of
--            CD GoRoot -> (EmptyDirTree, True, cs) -- unwind to top
--            CD GoParent -> (EmptyDirTree, False, cs) -- unwind one directory
--            CD (GoDir str) -> evalCommands cs (fromMaybe fileTree (find ((== str) . getName) (getChildren fileTree)))
--            LS strings -> evalCommands cs 
--                ---(
--                ---    me@(DirNode (Dir name children size parent)) -> DirNode (Dir name (parseLs parent strings) size (Just me))
--                ---)
--
----evalCommand :: Command -> (DirTree, Bool, 
--
----data Dir = Dir String [Node] (Maybe Int) (Maybe Node)
----data CDTarget = GoParent | GoDir String | GoRoot deriving (Show)
----data Command = CD CDTarget | LS [String]  deriving (Show)
--
----buildFileSystem :: [String] -> Node
----buildFileSystem inputs = DirNode $ Dir "/" [] Nothing Nothing
--
--parseLines :: [String] -> [Command]
--parseLines = parseLines' []
--
--parseLines' :: [Command] -> [String] -> [Command]
--parseLines' acc [] = reverse acc
--parseLines' acc (l:ls) = parseLines' (command:acc) remainingInput
--    where
--        (command, remainingInput) = parseCommand l ls
--
--parseCommand :: String -> [String] -> (Command, [String])
--parseCommand input nextInputs = if length input >= 4 && take 4 input == "$ ls"
--    then (LS commandLines, remainingInput)
--    else case cdTargetMatch !! 1 of
--        ".." -> (CD GoParent, nextInputs)
--        "/" -> (CD GoRoot, nextInputs)
--        _ -> (CD (GoDir (cdTargetMatch !! 1)), nextInputs)
--    where
--        cdTargetMatch = getAllTextSubmatches (input =~ "\\$ cd (.*)") :: [String]
--        (commandLines, remainingInput) = parseCommandOutputs nextInputs []
--
---- read lines for a command until end of list
---- or the beginning of another command
---- returns the strings that represent the outputs of the parsed command,
---- and the remaining input
--parseCommandOutputs :: [String] -> [String] -> ([String], [String])
--parseCommandOutputs [] linesForCommand = (reverse linesForCommand, [])
--parseCommandOutputs input@(x:xs) linesForCommand = if isCommand x
--    then (linesForCommand, input)
--    else parseCommandOutputs xs (x:linesForCommand)
--
--isCommand :: String -> Bool
--isCommand input = not (null input) && head input == '$'
--
----parseLs :: [String] -> [Dir]
----parseLs inputStrings = map parseNode inputStrings
--
----parseNode :: String -> DirTree
----parseNode input = if length input >= 4 && take 3 input == "dir"
----    then DirNode (Dir (drop 4 input) [] Nothing parent)
----    else FileNode (File (matches !! 2) (read (matches !! 1) :: Int))
----        where
----            matches = getAllTextSubmatches (input =~ "([0-9]+) ([a-z.]+)") :: [String]
