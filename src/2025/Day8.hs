module Day8 where

import Data.List (scanl', sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec as P (char, digit, many1, newline, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)
import qualified Utils.UnionFind as UF (clusterMap, fromList, unionClusters)

-- this is the kruskal algo one I think
-- I will use my UnionFind data structure

type Coords = (Int, Int, Int)

part1 :: IO Int
part1 = do
  input <- getInput

  let pairs = sortOn (uncurry getSquaredDist) . allPairs $ input
  let uf = UF.fromList input

  let res =
        last
          . take 1000
          . map (product . take 3 . sortOn (\x -> (-1) * x) . M.elems . M.map S.size . UF.clusterMap)
          . scanl' (\clusters (c1, c2) -> UF.unionClusters clusters c1 c2) uf
          $ pairs

  return res

part2 :: IO Int
part2 = do
  input <- getInput

  let pairs = sortOn (uncurry getSquaredDist) . allPairs $ input
  let uf = UF.fromList input
  let index =
        length
          . takeWhile ((/= 1) . M.size . UF.clusterMap)
          . scanl' (\clusters (c1, c2) -> UF.unionClusters clusters c1 c2) uf
          $ pairs

  let ((x1, _, _), (x2, _, _)) = pairs !! (index - 1)

  return (x1 * x2)

getSquaredDist :: Coords -> Coords -> Int
getSquaredDist (x1, y1, z1) (x2, y2, z2) =
  (x1 - x2) * (x1 - x2)
    + (y1 - y2) * (y1 - y2)
    + (z1 - z2) * (z1 - z2)

allPairs :: [a] -> [(a, a)]
allPairs xs = [(c1, c2) | (i, c1) <- ys, (j, c2) <- ys, i < j]
  where
    ys = zip [0 :: Int ..] xs

getInput :: IO [Coords]
getInput = unsafeParse (lineParser `P.sepBy` P.newline) <$> readFile "./fixtures/input8.txt"
  where
    lineParser :: Parser Coords
    lineParser = (,,) <$> intParser <* P.char ',' <*> intParser <* P.char ',' <*> intParser

    intParser :: Parser Int
    intParser = read <$> many1 digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res
