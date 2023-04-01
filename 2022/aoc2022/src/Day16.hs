module Day16 where


import Text.Parsec

import Parsing (
    digits
    )

data Valve = Valve String Int [String]
    deriving (Show)

run :: FilePath -> IO ()
run filepath = do
    print $ "day16, file path:" ++ filepath
    filecontent <- readFile filepath
    case runParser Day16.parse () filepath filecontent of
        Left e -> print $ "parsing error:" ++ show e
        Right parseResult -> do
            print $ "parse result:" ++ show parseResult

valveStatement :: Parsec String () Valve
valveStatement = do
    _ <- string "Valve" <* space
    valveName <- many letter <* space
    _ <- string "has flow rate="
    flowrate <- digits
    _ <- char ';' <* space
    _ <- string "tunnels lead to valve"
    _ <- optional (char 's') <* space
    tunnels <- many letter `sepBy1` string ", "
    return $ Valve valveName flowrate tunnels

parse :: Parsec String () [Valve]
parse = many (valveStatement <* endOfLine) <* eof
