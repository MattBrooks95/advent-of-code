module Parsing (
    comma
    , digits
    , plainWhitespace
    , equals
    , integer
    , plural
    ) where

import Text.Parsec

comma :: Parsec String () Char
comma = char ','

equals :: Parsec String () Char
equals = char '='

integer :: Parsec String () Int
--integer = read <$> ((option "" (string "-")) <$> many1 digit)
--integer = option "" (string "-") >>= \minus -> many1 digit >>= \nums -> return $ read (minus ++ nums)
integer = do
    minus <- option "" (string "-")
    parsedDigits <- many1 digit
    return $ read (minus ++ parsedDigits)

digits :: Parsec String () Int
digits = read <$> many1 digit

plainWhitespace :: Parsec String () Char
plainWhitespace = char ' '

plural :: Parsec String () ()
plural = optional $ char 's'
