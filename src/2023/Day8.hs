module Day8 where

import Data.Foldable
import Data.Map qualified as M (Map, fromList, (!))
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- 0.04 secs
-- 18113
solve :: IO (Maybe (Integer, String))
solve = do
  xs <- getLines "./fixtures/input8.txt"
  let infinteSeq = concat . repeat . head $ xs
  let nodes = M.fromList . map (unsafeParse branchParser) . drop 2 $ xs
  return . find ((== "ZZZ") . snd) . zip [0 ..] . go nodes $ infinteSeq

data Branch = Branch {left :: String, right :: String} deriving (Show)

go :: M.Map String Branch -> String -> [String]
go mp = scanl step "AAA"
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
