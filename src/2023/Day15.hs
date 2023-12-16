module Day15 where

import Data.Char (ord)
import Data.List.Split (splitOn)

-- 506869
-- 0.03 secs
part1 :: IO Int
part1 = do
  inp <- readFile "./fixtures/input15.txt"
  return . sum . map hash . splitOn "," $ inp

hash :: String -> Int
hash = foldl (\n c -> (`rem` 256) . (* 17) . (+ n) . ord $ c) 0