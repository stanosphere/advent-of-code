module Day2 where

import Data.Foldable (find)
import Data.List.Split (splitOn)
import Data.Map
  ( Map,
    fromList,
    insert,
    (!),
  )
import Data.Maybe
  ( catMaybes,
    isJust,
  )

data SystemState = SS
  { state :: Map Int Int,
    nextPosition :: Int
  }

part1 :: IO ()
part1 = do
  raw <- input
  print (solve raw [(1, 12), (2, 2)])

part2 :: IO ()
part2 = do
  raw <- input
  print
    . fmap (\lst -> 100 * snd (head lst) + snd (lst !! 1))
    . find ((== 19690720) . solve raw)
    $ [[(1, i), (2, j)] | i <- [0 .. 99], j <- [0 .. 99]]

solve :: [Int] -> [(Int, Int)] -> Int
solve raw replacements =
  (! 0)
    . state
    . last
    . iterateUntilNothing runStep
    . mkInitState replacements
    $ raw

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing f = catMaybes . takeWhile isJust . iterateMaybe f

iterateMaybe :: (a -> Maybe a) -> a -> [Maybe a]
iterateMaybe f init = iterate g (Just init)
  where
    g (Just x) = f x
    g Nothing = Nothing

runStep :: SystemState -> Maybe SystemState
runStep (SS state i) =
  let opCode = state ! i
      input1 = state ! (state ! (i + 1))
      input2 = state ! (state ! (i + 2))
      positionToChange = state ! (i + 3)
      op = parseOpCode opCode
   in fmap
        (\f -> SS (insert positionToChange (f input1 input2) state) (i + 4))
        op
  where
    parseOpCode 1 = Just (+)
    parseOpCode 2 = Just (*)
    parseOpCode _ = Nothing

mkInitState :: [(Int, Int)] -> [Int] -> SystemState
mkInitState replacements ints =
  SS
    (foldl insertReplacement mapFromList replacements)
    0
  where
    insertReplacement acc (k, v) = insert k v acc
    mapFromList = fromList . zip [0 ..] $ ints

input :: IO [Int]
input = fmap parseRawInput . readFile $ "./fixtures/input2.txt"

parseRawInput :: String -> [Int]
parseRawInput = map read . splitOn "," . head . lines
