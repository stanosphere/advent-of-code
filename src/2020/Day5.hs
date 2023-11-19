module Day4 where

import Data.Foldable
import Data.List (sortOn, tails)

type Range = (Int, Int)

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input5.txt"
  print . maximum . map (toSeatId . decode) $ contents
  print ()

-- BFFFBBFRRR: row 70, column 7, seat ID 567.
-- FFFBBBFRRR: row 14, column 7, seat ID 119.
-- BBFFBBFRLL: row 102, column 4, seat ID 820.

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input5.txt"
  let seatIds = sortOn id . map (toSeatId . decode) $ contents
  let xs = filter (\(x1, x2) -> x2 /= x1 + 1) . pairs $ seatIds
  traverse_ print xs

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toSeatId :: (Int, Int) -> Int
toSeatId (row, col) = row * 8 + col

decode :: String -> (Int, Int)
decode s =
  let row = decodeRow . take 7 $ s
      col = decodeCol . drop 7 $ s
   in (row, col)

decodeRow :: String -> Int
decodeRow = fst . foldl step (0, 127)
  where
    step :: Range -> Char -> Range
    step (x, y) 'F' = (x, y - ((y - x + 1) `div` 2))
    step (x, y) 'B' = (x + ((y - x + 1) `div` 2), y)
    step _ _ = undefined

decodeCol :: String -> Int
decodeCol = fst . foldl step (0, 7)
  where
    step :: Range -> Char -> Range
    step (x, y) 'L' = (x, y - ((y - x + 1) `div` 2))
    step (x, y) 'R' = (x + ((y - x + 1) `div` 2), y)
    step _ _ = undefined

pairs :: [a] -> [(a, a)]
pairs = map (\[x1, x2] -> (x1, x2)) . windows 2

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails