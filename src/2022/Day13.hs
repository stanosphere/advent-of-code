module Day13 where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.List.Split (chunksOf)
import Text.Parsec qualified as P
  ( char,
    digit,
    many1,
    parse,
    sepBy,
    (<|>),
  )
import Text.Parsec.String (Parser)

data Tree = Value Int | List [Tree] deriving (Show, Eq)

instance Ord Tree where
  compare :: Tree -> Tree -> Ordering
  compare (Value x) (Value y) = compare x y
  compare (List xs) (List ys) = compare xs ys
  compare (Value x) (List ys) = compare (List [Value x]) (List ys)
  compare (List xs) (Value y) = compare (List xs) (List [Value y])

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input13.txt"
  print
    . countCorrectlyOrderedPairs
    . map (\[x1, x2] -> parseBothTrees (x1, x2))
    . map (take 2)
    . chunksOf 3
    $ input

part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input13.txt"
  traverse_ print . findIndexes . map parseTree . filter (/= "") $ input

countCorrectlyOrderedPairs :: [(Tree, Tree)] -> Int
countCorrectlyOrderedPairs = sum . map fst . filter (\(_, (t1, t2)) -> t1 < t2) . zip [1 ..]

findIndexes :: [Tree] -> [(Int, Tree)]
findIndexes =
  filter ((`elem` additionalTrees) . snd)
    . zip [1 ..]
    . sort
    . (additionalTrees ++)
  where
    additionalTrees = [List [List [Value 6]], List [List [Value 2]]]

-- Parsing stuff below here
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case P.parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

treeParser :: Parser Tree
treeParser = valueParser P.<|> childrenParser
  where
    valueParser = Value <$> intParser
    childrenParser = List <$> (P.char '[' *> P.sepBy treeParser (P.char ',') <* P.char ']')

parseTree :: String -> Tree
parseTree = unsafeParse treeParser

parseBothTrees :: (String, String) -> (Tree, Tree)
parseBothTrees (s1, s2) = (parseTree s1, parseTree s2)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)