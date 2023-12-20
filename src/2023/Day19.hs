{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day19 where

import Data.Map qualified as M (Map, fromList)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

type Part = M.Map Char Int

data Destination = Accept | Reject | ToWorkFlow String deriving (Show)

data Rule = Rule {condition :: Condition, destination :: Destination} deriving (Show)

data WorkFlow = WF {id :: String, rules :: [Rule]} deriving (Show)

data Op = GreaterThan | LessThan deriving (Show)

data Condition = Cond {partType :: Char, op :: Op, comparator :: Int} | Default deriving (Show)

-- need function that maps a condtioon to a function..

part1 = unsafeParse partParser "{x=2036,m=264,a=79,s=2244}"

-- lots of parsing for thi one!
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

workFlowParser :: Parser WorkFlow
workFlowParser = do
  wId <- P.many1 P.letter
  _ <- P.char '{'
  rules <- P.sepBy ruleParser (P.char ',')
  return (WF wId rules)

ruleParser :: Parser Rule
ruleParser = conditionedRuleParser <|> conditionedRuleParser

defaultRuleParser :: Parser Rule
defaultRuleParser = Rule Default <$> destinationParser

conditionedRuleParser :: Parser Rule
conditionedRuleParser = do
  cond <- conditionParser
  _ <- P.char ':'
  dest <- destinationParser
  return (Rule cond dest)

conditionParser :: Parser Condition
conditionParser = do
  partType <- P.oneOf "xmas"
  op <- opParser
  comparator <- intParser
  return (Cond partType op comparator)

opParser :: Parser Op
opParser = (GreaterThan <$ P.char '>') <|> (LessThan <$ P.char '<')

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

destinationParser :: Parser Destination
destinationParser =
  (Accept <$ P.char 'A')
    <|> (Reject <$ P.char 'R')
    <|> (ToWorkFlow <$> P.many1 (P.noneOf "RA"))

-- {x=2036,m=264,a=79,s=2244}
partParser :: Parser Part
partParser = M.fromList <$> (P.char '{' *> P.sepBy tupleParser (P.char ',') <* P.char '}')

tupleParser :: Parser (Char, Int)
tupleParser = do
  partId <- P.oneOf "xmas"
  _ <- P.char '='
  partRating <- intParser
  return (partId, partRating)