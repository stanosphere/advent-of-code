module Day13 where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Data.Ord (Ordering)
import GHC.Data.Maybe (rightToMaybe)
import Text.Parsec
  ( char,
    choice,
    digit,
    many1,
    parse,
    sepBy,
  )
import Text.Parsec.String (Parser)

data Tree = Value Int | List [Tree] deriving (Show, Eq)

num :: Parser Int
num = read <$> many1 digit

-- this parser is STOLEN
tree :: Parser Tree
tree =
  ( choice
      [Value <$> num, List <$> (char '[' *> sepBy tree (char ',') <* char ']')]
  )

parseTree :: String -> Maybe Tree
parseTree = rightToMaybe . parse tree ""

parseBothTrees :: (String, String) -> Maybe (Tree, Tree)
parseBothTrees (s1, s2) = do
  t1 <- parseTree s1
  t2 <- parseTree s2
  return (t1, t2)

instance Ord Tree where
  compare (Value x) (Value y) = compare x y
  compare (List xs) (List ys) = compare xs ys
  compare (Value x) (List ys) = compare (List [Value x]) (List ys)
  compare (List xs) (Value y) = compare (List xs) (List [Value y])

part1 :: IO ()
part1 = do
  lines <- getLines "./fixtures/input13.txt"
  let xs =
        traverse (\[x1, x2] -> parseBothTrees (x1, x2))
          . map (take 2)
          . chunksOf 3
          $ lines
  case xs of
    Nothing -> print "oops"
    Just ys -> print . countCorrectlyOrderedPairs $ ys

part2 :: IO ()
part2 = do
  lines <- getLines "./fixtures/input13.txt"
  let xs = traverse (parseTree) . filter (\x -> x /= "") $ (lines)
  case xs of
    Nothing -> print "oops"
    Just trees -> traverse_ print . findIndexes $ trees

countCorrectlyOrderedPairs :: [(Tree, Tree)] -> Int
countCorrectlyOrderedPairs =
  sum . map fst . filter (\(i, (t1, t2)) -> compare t1 t2 == LT) . zip [1 ..]

findIndexes :: [Tree] -> [(Int, Tree)]
findIndexes =
  let additionalTrees = [List [List [Value 6]], List [List [Value 2]]]
   in filter (\(i, tree) -> tree `elem` additionalTrees)
        . zip [1 ..]
        . sort
        . (additionalTrees ++)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
