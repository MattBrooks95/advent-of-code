module Day16 where


import Text.Parsec

import Parsing (
    digits
    , plainWhitespace
    , plural
    )

data Valve = Valve String Int [String]
    deriving (Show)

run :: FilePath -> IO ()
run filepath = do
    print $ "day16, file path:" ++ filepath
    filecontent <- readFile filepath
    case runParser Day16.parse () filepath filecontent of
        Left e -> do
            print "parsing error"
            print e
        Right parseResult -> do
            print $ "parse result:" ++ show parseResult

valveStatement :: Parsec String () Valve
valveStatement = do
    _ <- string "Valve" <* space
    valveName <- many letter <* space
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
    return $ Valve valveName flowrate tunnels

parse :: Parsec String () [Valve]
parse = many (valveStatement <* endOfLine) <* eof
