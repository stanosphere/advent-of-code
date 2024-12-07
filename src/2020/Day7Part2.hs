module Day7Part2 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- hmm I did part 1 like agesssss ago
-- think I'l just try part 2 from scratch

-- so a rule might look like
-- drab yellow bags contain 4 light chartreuse bags, 3 striped crimson bags, 2 faded gray bags.

type BagType = String

data Rule = Rule
  { _outer :: BagType,
    inner :: [(BagType, Int)]
  }
  deriving (Show)

getInput :: IO [Rule]
getInput = map (unsafeParse ruleParser) . lines <$> readFile "./fixtures/input7.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

ruleParser :: Parser Rule
ruleParser = do
  outer <- P.manyTill bagTypeCharParser (P.try . P.string $ " bags contain ")
  inners <- catMaybes <$> P.sepBy bagParser (P.string ", ")
  _ <- P.char '.'
  return (Rule outer inners)
  where
    bagParser :: Parser (Maybe (BagType, Int))
    bagParser = P.try nonEmptyBagParser P.<|> emptyBagParser

    emptyBagParser :: Parser (Maybe (BagType, Int))
    emptyBagParser = P.string "no other bags" $> Nothing

    nonEmptyBagParser :: Parser (Maybe (BagType, Int))
    nonEmptyBagParser = do
      n <- intParser
      _ <- P.char ' '
      bagType <- P.manyTill bagTypeCharParser bagStatementEndParser
      return . Just $ (bagType, n)
      where
        -- annoyingly if there's one bag it ends in "bag rather than bags!"
        bagStatementEndParser = (P.try . P.string $ " bags") P.<|> (P.try . P.string $ " bag")

    bagTypeCharParser :: Parser Char
    bagTypeCharParser = P.letter P.<|> P.char ' '

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit