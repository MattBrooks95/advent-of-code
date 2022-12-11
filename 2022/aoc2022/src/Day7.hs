module Day7 where

import Prelude hiding (
    lines
    )

import Data.Maybe (
    fromMaybe
    )

import Data.List (
    intercalate
    )

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

data CDTarget = GoParent | GoDir String | GoRoot String
data Command = CD CDTarget | LS [String]

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

buildFileSystem :: [String] -> Node
buildFileSystem inputs = DirNode $ Dir "/" [] Nothing Nothing

parseLines :: [String] -> [Command]
parseLines = parseLines' []

parseLines' :: [Command] -> [String] -> [Command]
parseLines' acc [] = reverse acc
parseLines' acc (l:ls) = [CD (GoRoot "\\")]
