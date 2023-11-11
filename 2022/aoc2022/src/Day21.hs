module Day21 (
    run
    , MonkeyType(..)
    , Monkey(..)
    , Expression(..)
    , parseLiteralMonkey
    , Op(..)
    , parseExpressionMonkey
    , MonkeyName(..)
    , parseMonkey
    ) where

import System.Exit (
    die
    )

import System.FilePath (
    FilePath
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString as BSL
import qualified Data.ByteString as BSS (
    readFile
    )
import qualified Data.Word8 as W
import qualified Data.Attoparsec.ByteString.Lazy as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import Control.Applicative ((<|>))

run :: FilePath -> IO ()
run fp = do
    fileContents <- BSS.readFile fp
    parseResult <- do
        case AP.parseOnly parse (BSL.fromStrict fileContents) of
            Left err -> die ("failed to parse" <> err)
            Right pr -> pure pr
    print parseResult
    print "Day21"

newtype MonkeyName = MonkeyName BSL.ByteString
    deriving (Show, Eq)

data Op = Add | Sub | Mult | Div deriving (Show, Eq)

-- | it looks like Monkeys that have expressions can't do math with literals, on the result of evaluating other monkeys
data Expression = Expression {
    expArg1 :: MonkeyName
    , expOp :: Op
    , expArg2 :: MonkeyName
    }
    deriving (Show, Eq)

data MonkeyType = Evaluated Double
    | LiteralMonkey Int
    | ExpressionMonkey Expression
    deriving (Show, Eq)

-- n is the string type for the monkey name
-- well, I wanted it to be parameterized but I'm not sure how I can do this and
-- derive Show
--data Monkey n = (Eq n) => Monkey n MonkeyType
--deriving instance Show (Monkey BS.ByteString)
data Monkey = Monkey MonkeyName MonkeyType
    deriving (Show, Eq)

parse :: AP.Parser [Monkey]
parse = AP.sepBy' parseMonkey APC.endOfLine

parseMonkey :: AP.Parser Monkey
parseMonkey = do
    monkeyName <- (MonkeyName <$> AP.takeTill (== W._colon)) AP.<?> "parse monkey name"
    _ <- AP.word8 W._colon
    _ <- AP.word8 W._space
    monkeyValue <- (parseLiteralMonkey <|> parseExpressionMonkey) AP.<?> "parse monkey value"
    pure $ Monkey monkeyName monkeyValue
    AP.<?> "parseMonkey"

parseLiteralMonkey :: AP.Parser MonkeyType
parseLiteralMonkey = LiteralMonkey <$> APC.decimal

parseExpressionMonkey :: AP.Parser MonkeyType
parseExpressionMonkey = ExpressionMonkey <$> parseExpression

skipSpace :: AP.Parser ()
skipSpace = AP.skip (== W._space)

parseExpression :: AP.Parser Expression
parseExpression = Expression <$>
    (parseMonkeyNameInExp <* skipSpace)
    <*> (parseOp <* skipSpace)
    <*> parseMonkeyNameInExp
    AP.<?> "parseExpression"

parseMonkeyNameInExp :: AP.Parser MonkeyName
parseMonkeyNameInExp = MonkeyName
    <$> AP.takeWhile (AP.inClass "a-z")
    AP.<?> "parseMonkeyNameInExp"

parseOp :: AP.Parser Op
parseOp =  AP.choice [AP.word8 W._plus >> pure Add
    , AP.word8 W._hyphen >> pure Sub
    , AP.word8 W._asterisk >> pure Mult
    , AP.word8 W._slash >> pure Div
    ] APC.<?> "parse operator" 
