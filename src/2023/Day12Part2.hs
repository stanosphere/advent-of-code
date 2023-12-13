{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- this is chiefly inspired by https://github.com/ColonelPhantom/aoc2023/blob/main/Day12.hs
-- but I didn't quite understand the array stuff so I used a memosation technique that's more familiar to me
-- which I took from here https://gist.github.com/beala/d871ae8397167e7035f218a25ddf87dd
-- Essentially I knew my part 1 wasn't good enough so I sought better answers!
-- probably don't really deserve my 2nd star lol
-- but I've learnt a generic way to memoize in Haskell which is useful
-- indeed it's rather similar to what I would do in scala
-- except in scala I would usually just have the map as a top level var
-- also one wonders if the computation would be quicker if I used the same map between all computations
-- -- this should be fairly straightforward to implement actually, might have a go!

module Day12Part2 where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Foldable (traverse_)
import Data.List.Extra (intercalate, (!?))
import Data.Map.Strict as Map (Map, empty, insert, lookup)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- 7857
part1 :: IO Int
part1 = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map (uncurry goMemEval . parseLine) xs
  traverse_ print res
  return . sum $ res

part1UnMemoized :: IO Int
part1UnMemoized = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map (uncurry go . parseLine) xs
  traverse_ print res
  return . sum $ res

-- 28606137449920
part2 :: IO Int
part2 = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map (uncurry goMemEval . unfoldInstructions . parseLine) xs
  traverse_ print res
  return . sum $ res

-- not gonna finish
part2UnMemoized :: IO Int
part2UnMemoized = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map (uncurry go . unfoldInstructions . parseLine) xs
  traverse_ print res
  return . sum $ res

goMemEval :: [Int] -> String -> Int
goMemEval s is = evalState (goMem s is) Map.empty

-- Ask for a value in the cache. If it's not there, run the state computation
-- and insert the result into the cache.
getOrGoMem :: [Int] -> String -> State (Map ([Int], String) Int) Int
getOrGoMem gs springs = getOrUpdate (gs, springs) (goMem gs springs)
  where
    getOrUpdate :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
    getOrUpdate k ifEmptyState = do
      maybeVal <- gets (Map.lookup k)
      case maybeVal of
        Just v -> return v
        Nothing -> do
          ifEmpty <- ifEmptyState
          modify (Map.insert k ifEmpty)
          return ifEmpty

-- this is the momoised version, note the similarity with `go`
goMem :: [Int] -> String -> State (Map.Map ([Int], String) Int) Int
goMem [] [] = return 1 -- run out of both springs and groups at same time: valid combo
goMem _ [] = return 0 -- run out of springs but not not run out of groups yet
goMem [] ('#' : _) = return 0 -- run out of groups but still have a damaged spring
goMem springs ('.' : gs) = getOrGoMem springs gs
goMem springs ('?' : gs) = do
  x <- getOrGoMem springs ('.' : gs) -- I suppose I could just directly jump to using `gs` rather than `'.' : gs` but I quite enjoy the symmetry
  y <- getOrGoMem springs ('#' : gs)
  return (x + y)
goMem (g : gs) xs = if couldBeValid xs g then getOrGoMem gs (drop g . drop 1 $ xs) else return 0

-- here's the un-memoized version
go :: [Int] -> String -> Int
go [] [] = 1 -- run out of both springs and groups at same time: valid combo
go _ [] = 0 -- run out of springs but not not run out of groups yet
go [] ('#' : _) = 0 -- run out of groups but still have a damaged spring
go springs ('.' : gs) = go springs gs
go springs ('?' : gs) = go springs ('.' : gs) + go springs ('#' : gs)
go (g : gs) xs = if couldBeValid xs g then go gs (drop g . drop 1 $ xs) else 0

couldBeValid :: String -> Int -> Bool
couldBeValid springs groupSize =
  head springs == '#'
    && length springs >= groupSize -- have to have enough springs left for the size of the group
    && (notElem '.' . take groupSize $ springs) -- all springs must be damaged
    && (springs !? groupSize /= Just '#') -- after the group we must not have a damaged string

unfoldInstructions :: ([Int], String) -> ([Int], String)
unfoldInstructions (groups, springs) = (concat . replicate 5 $ groups, intercalate "?" . replicate 5 $ springs)

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