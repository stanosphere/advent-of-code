module Day3 where

import Data.Char (digitToInt)
import Data.List (intercalate)

data Diagnostic = Diag {gamma :: Int, epsilon :: Int}

testData :: [String]
testData = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

part1 :: IO ()
part1 = do
  lines <- getLines "./fixtures/input3.txt"
  let res = foldData . map lineToDigits $ lines
  print (getGamma res * getEpsilon res)

part2 :: IO ()
part2 = do
  lines <- getLines "./fixtures/input3.txt"
  -- let res = oxygen 0 . map lineToDigits $ lines
  let oxygenRes = toDec . intercalate "" . map show . head . oxygen 0 . map lineToDigits $ lines
  let carbonRes = toDec . intercalate "" . map show . head . carbonDioxide 0 . map lineToDigits $ lines
  print oxygenRes
  print carbonRes
  print (oxygenRes * carbonRes)

type Position = Int

oxygen :: Position -> [[Int]] -> [[Int]]
oxygen position xss =
  let (zeroCount, oneCount) = getCountsAtPosition position xss
      keeper = if zeroCount > oneCount then 0 else 1
      keptRows = filter (\row -> (row !! position) == keeper) xss
   in if length keptRows == 1 then keptRows else oxygen (position + 1) keptRows

carbonDioxide :: Position -> [[Int]] -> [[Int]]
carbonDioxide position xss =
  let (zeroCount, oneCount) = getCountsAtPosition position xss
      keeper = if zeroCount <= oneCount then 0 else 1
      keptRows = filter (\row -> (row !! position) == keeper) xss
   in if length keptRows == 1 then keptRows else carbonDioxide (position + 1) keptRows

getCountsAtPosition :: Position -> [[Int]] -> (Int, Int)
getCountsAtPosition position = foldl updateCounts (0, 0) . map (!! position)

getPowerConsumption :: ((Int, Int) -> String) -> [(Int, Int)] -> Int
getPowerConsumption f = toDec . intercalate "" . map f

getEpsilon :: [(Int, Int)] -> Int
getEpsilon = getPowerConsumption (\(x, y) -> if x > y then "1" else "0")

getGamma :: [(Int, Int)] -> Int
getGamma = getPowerConsumption (\(x, y) -> if x > y then "0" else "1")

lineToDigits :: [Char] -> [Int]
lineToDigits = map digitToInt

foldData :: [[Int]] -> [(Int, Int)]
foldData = foldl (zipWith updateCounts) zero

updateCounts :: (Int, Int) -> Int -> (Int, Int)
updateCounts (a, b) 0 = (a + 1, b)
updateCounts (a, b) 1 = (a, b + 1)
updateCounts x _ = x

zero :: [(Int, Int)]
zero = replicate 12 (0, 0)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0
