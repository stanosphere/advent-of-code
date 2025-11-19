module Day8 where

import Data.Foldable (Foldable (foldl'))
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Text.Parsec as P (char, choice, digit, letter, many1, newline, sepBy, string, try, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)
import Prelude hiding (EQ, GT, LT)

data Operand = EQ | NEQ | LT | LEQ | GT | GEQ deriving (Show)

data Action = Inc | Dec deriving (Show)

-- I suppose this could be represented as a function, but I like being able to see the operand I guess
-- like the parser absolutely could output a function with signature `Int -> Bool` given the comparator and operand
data Condition = Condition {toCheck :: String, operand :: Operand, comparator :: Int} deriving (Show)

-- again we probably don't need to store action and amount separately
-- but a parser should probably just parse, and not have any other logic
data Command = Command
  { toModify :: String,
    action :: Action,
    amount :: Int,
    cond :: Condition
  }
  deriving (Show)

type Register = M.Map String Int

part1 :: IO Int
part1 = foldr max 0 . foldl' applyCommand M.empty <$> getInput

applyCommand :: Register -> Command -> Register
applyCommand reg command = if res then M.insertWith (+) (toModify command) valueToInsert reg else reg
  where
    condition = cond command
    existingValue = fromMaybe 0 (reg M.!? toCheck condition)
    comparatorValue = comparator condition
    res = case operand condition of
      EQ -> existingValue == comparatorValue
      NEQ -> existingValue /= comparatorValue
      LT -> existingValue < comparatorValue
      LEQ -> existingValue <= comparatorValue
      GT -> existingValue > comparatorValue
      GEQ -> existingValue >= comparatorValue
    valueToInsert = case action command of
      Inc -> amount command
      Dec -> (-1) * amount command

getInput :: IO [Command]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input8.txt"
  where
    inputParser = commandParser `P.sepBy` P.newline

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

    -- order here matters: you have to try `<=` before plain old `<` for example
    operandParser :: Parser Operand
    operandParser =
      P.choice . map P.try $
        [ P.string "==" $> EQ,
          P.string "!=" $> NEQ,
          P.string "<=" $> LEQ,
          P.string "<" $> LT,
          P.string ">=" $> GEQ,
          P.string ">" $> GT
        ]

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res
