module Day8 where

import Data.Functor (($>))
import Text.Parsec as P (char, choice, digit, letter, many1, string, try, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)
import Prelude hiding (EQ, GT, LT)

data Operand = EQ | LT | LEQ | GT | GEQ deriving (Show)

data Action = Inc | Dec deriving (Show)

data Condition = Condition {toCheck :: String, operand :: Operand, comparator :: Int} deriving (Show)

data Command = Command
  { toModify :: String,
    action :: Action,
    amount :: Int,
    cond :: Condition
  }
  deriving (Show)

commandParser :: Parser Command
commandParser =
  Command
    <$> idParser
    <* P.char ' '
    <*> actionParser
    <* P.char ' '
    <*> intParser
    <* P.string " if "
    <*> conditionParser
  where
    intParser = read <$> P.many1 (P.digit <|> P.char '-')
    idParser = many1 P.letter

    actionParser :: Parser Action
    actionParser = P.choice . map P.try $ [P.string "inc" $> Inc, P.string "dec" $> Dec]

    conditionParser :: Parser Condition
    conditionParser =
      Condition
        <$> idParser
        <* P.char ' '
        <*> operandParser
        <* P.char ' '
        <*> intParser

    operandParser :: Parser Operand
    operandParser =
      P.choice . map P.try $
        [ P.string "==" $> EQ,
          P.string "<" $> LT,
          P.string "<=" $> LEQ,
          P.string ">" $> GT,
          P.string ">=" $> GEQ
        ]

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res
