module Day14 where

import Data.Foldable (traverse_)
import Data.List (groupBy, sort, transpose)
import Data.Map qualified as M (elems)
import Utils.Grouping (groupMap)

part1 = do
  inp <- getLines "./fixtures/input14.txt"
  return . sum . zipWith (\i r -> i * countRoundRocks r) [1 ..] . reverse . shiftNorth $ inp

-- I conjecture that the cycle probably repeats itself
-- Indeed the toy example has a 7-fold repeat pattern after 3 iterations
-- I note that the real example has a 9-fold repeat pattern which starts after 92 cycles
-- I've pulled how I know this out as a function
determineRepeatStructure :: IO ()
determineRepeatStructure =
  getLines "./fixtures/input14Toy.txt"
    >>= traverse_ print
      . M.elems
      . groupMap snd fst
      . take 130
      . zip [(0 :: Int) ..]
      . iterate applyCycle

part2 = do
  inp <- getLines "./fixtures/input14Toy.txt"

  let cycles = M.elems . groupMap snd fst . take 200 . zip [0 ..] . iterate applyCycle $ inp

  traverse_ print cycles

applyCycle :: [String] -> [String]
applyCycle = shiftEast . shiftSouth . shiftWest . shiftNorth

shiftNorth :: [String] -> [String]
shiftNorth = reverse . transpose . map processRow . transpose . reverse

shiftSouth :: [String] -> [String]
shiftSouth = reverse . transpose . map processRow' . transpose . reverse

shiftWest :: [String] -> [String]
shiftWest = map processRow'

shiftEast :: [String] -> [String]
shiftEast = map processRow

processRow :: String -> String
processRow = concatMap processGroup . groupBy groupFn
  where
    groupFn x y = (x == '#' && y == '#') || (x /= '#' && y /= '#')
    processGroup g = if head g == '#' then g else sort g

processRow' :: String -> String
processRow' = concatMap processGroup . groupBy groupFn
  where
    groupFn x y = (x == '#' && y == '#') || (x /= '#' && y /= '#')
    processGroup g = if head g == '#' then g else reverse . sort $ g

countRoundRocks :: [Char] -> Int
countRoundRocks = length . filter (== 'O')

getLines :: FilePath -> IO [String]
getLines filePath = lines <$> readFile filePath