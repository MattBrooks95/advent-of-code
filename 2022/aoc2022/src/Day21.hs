module Day21 (
    run
    ) where

import System.FilePath (
    FilePath
    )

import Data.ByteString as BSS (
    readFile
    )
import qualified Data.Attoparsec.ByteString.Lazy as AP

run :: FilePath -> IO ()
run fp = do
    fileContents <- BSS.readFile fp
    print "Day21"

newtype MonkeyName = MonkeyName String

data Op = Add | Sub | Mult | Div

-- | it looks like Monkeys that have expressions can't do math with literals, on the result of evaluating other monkeys
data Expression = Expression {
    expArg1 :: MonkeyName
    , expArg2 :: MonkeyName
    , expOp :: Op
    }

data Monkey = Evaluated Double | LiteralMonkey Int | ExpressionMonkey Expression

parse :: AP.Parser [Monkey]
parse = undefined
