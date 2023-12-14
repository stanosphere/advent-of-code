module Day14 where

import Data.Foldable (traverse_)
import Data.List (groupBy, sort, transpose)

part1 = do
  inp <- getLines "./fixtures/input14.txt"
  return . sum . zipWith (\i r -> i * countRoundRocks r) [1 ..] . reverse . shiftNorth $ inp

countRoundRocks :: [Char] -> Int
countRoundRocks = length . filter (== 'O')

getLines :: FilePath -> IO [String]
getLines filePath = lines <$> readFile filePath

shiftNorth :: [String] -> [String]
shiftNorth = reverse . transpose . map processRow . transpose . reverse

processRow :: String -> String
processRow = concatMap processGroup . groupBy (\x y -> (x == '#' && y == '#') || (x /= '#' && y /= '#'))
  where
    processGroup g = if head g == '#' then g else sort g
