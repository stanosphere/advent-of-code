module Day7Part2 where

import Data.Functor (($>))
import Data.List (find)
import Data.Maybe (catMaybes)
import Data.Tree (Tree, foldTree, unfoldTree)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- hmm I did part 1 like agesssss ago
-- think I'l just try part 2 from scratch

type BagType = String

data Rule = Rule {_outer :: BagType, _inner :: [(BagType, Int)]} deriving (Show)

-- have to subtract off the 1 shiny gold back at the end
part2 :: IO Int
part2 = (\x -> x - 1) . sumTree . buildTree <$> getInput

sumTree :: Tree (BagType, Int) -> Int
sumTree = foldTree (\(_, x) xs -> x * (1 + sum xs))

buildTree :: [Rule] -> Tree (BagType, Int)
buildTree xs = unfoldTree getChildren ("shiny gold", 1)
  where
    getChildren :: (BagType, Int) -> ((BagType, Int), [(BagType, Int)])
    getChildren (nodeLabel, n) =
      ( (nodeLabel, n),
        (maybe [] _inner . find ((== nodeLabel) . _outer)) xs
      )

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