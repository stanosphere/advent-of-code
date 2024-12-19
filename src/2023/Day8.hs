module Day8 where

import Data.Foldable (find)
import qualified Data.Map as M (Map, fromList, keys, (!))
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- 0.04 secs
-- 18113
solvePart1 :: IO (Maybe Integer)
solvePart1 = do
  xs <- getLines "./fixtures/input8.txt"
  let infiniteSeq = cycle . head $ xs
  let nodes = M.fromList . map (unsafeParse branchParser) . drop 2 $ xs
  return . fmap fst . find ((== "ZZZ") . snd) . zip [0 ..] . go "AAA" nodes $ infiniteSeq

-- hmm sooooo online they're all using LCM
-- but really in order to use it you should show that the cycles really are synchronised...
-- anyway I guess I'll just do that

-- I guess more precisely one would need to show that the distance from the start node to the end node is always equal to the distance from the end node to itself
-- which I can believe I would have discovered through playing around rather than naughtily looking at reddit

-- 12315788159977
-- 0.08 secs
solvePart2 :: IO Integer
solvePart2 = do
  xs <- getLines "./fixtures/input8.txt"
  let infiniteSeq = cycle . head $ xs
  let nodes = M.fromList . map (unsafeParse branchParser) . drop 2 $ xs
  return
    . foldl1 lcm
    . mapMaybe
      ( fmap fst
          . (find ((== 'Z') . last . snd) . zip [0 ..])
          . (\startingPoint -> go startingPoint nodes infiniteSeq)
      )
    . filter ((== 'A') . last)
    . M.keys
    $ nodes

data Branch = Branch {left :: String, right :: String} deriving (Show)

go :: String -> M.Map String Branch -> String -> [String]
go start mp = scanl step start
  where
    step :: String -> Char -> String
    step s 'R' = right (mp M.! s)
    step s 'L' = left (mp M.! s)
    step _ _ = undefined

-- parsing
branchParser :: Parser (String, Branch)
branchParser = do
  from <- P.many P.letter
  P.string " = ("
  left <- P.many P.letter
  P.string ", "
  right <- P.many P.letter
  P.string ")"
  return (from, Branch left right)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res
