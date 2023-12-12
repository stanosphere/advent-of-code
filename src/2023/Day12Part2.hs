{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- this is inspired by https://github.com/ColonelPhantom/aoc2023/blob/main/Day12.hs
-- I knew my part 1 wasn't good enough so I sought better answers!

module Day12Part2 where

import Control.Monad.State.Strict (State, gets, modify)
import Data.List.Extra ((!?))
import Data.Map.Strict as Map (Map, insert, lookup)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

part1 :: IO Int
part1 = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map ((uncurry . flip $ countValid) . parseLine) $ xs
  print res
  return . sum $ res

-- memoize :: (String -> [Int] -> a) -> (String -> [Int] -> a)
-- memoize f = (map f [0 ..] !!)

-- Ask for a value in the cache. If it's not there, run the state computation
-- and insert the result into the cache.
getOrUpdate :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (Map.insert k ifEmpty)
      return ifEmpty

countValid' :: String -> [Int] -> Int
countValid' = goMem
  where
    goMem :: String -> [Int] -> Int
    goMem [] [] = 1
    goMem [] _ = 0
    goMem ('#' : _) [] = 0
    goMem ('.' : gs) springs = goMem gs springs
    goMem ('?' : gs) springs = goMem ('.' : gs) springs + goMem ('#' : gs) springs
    goMem xs (g : gs) = if couldBeValid xs g then goMem (drop g . drop 1 $ xs) gs else 0

-- here's the un-memoized version
countValid :: String -> [Int] -> Int
countValid = go
  where
    go :: String -> [Int] -> Int
    go [] [] = 1
    go [] _ = 0
    go ('#' : _) [] = 0
    go ('.' : gs) springs = go gs springs
    go ('?' : gs) springs = go ('.' : gs) springs + go ('#' : gs) springs
    go xs (g : gs) = if couldBeValid xs g then go (drop g . drop 1 $ xs) gs else 0

couldBeValid :: String -> Int -> Bool
couldBeValid springs groupSize =
  head springs == '#'
    && length springs >= groupSize -- have to have enough springs left for the size of the group
    && notElem '.' (take groupSize springs) -- all springs must be damaged
    && (springs !? groupSize /= Just '#') -- after the group we must not have a damaged string

parseLine :: String -> ([Int], String)
parseLine = unsafeParse lineParser

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

lineParser :: Parser ([Int], String)
lineParser = do
  res <- P.many springParser
  _ <- P.space
  numbers <- P.sepBy intParser (P.char ',')
  return (numbers, res)

springParser :: Parser Char
springParser = P.oneOf "#.?"

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)