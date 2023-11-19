module Day7 where

import Data.List.Split (splitOn)

part1 :: IO ()
part1 = do
  input <- getInput "./fixtures/input7.txt"
  let res = minimum . map (getCostOfMovingCrabs input) $ [0 .. maximum input]
  print res

part2 :: IO ()
part2 = do
  input <- getInput "./fixtures/input7.txt"
  let res = minimum . map (getCostOfMovingCrabs' input) $ [0 .. maximum input]
  print res

getCostOfMovingCrabs :: [Integer] -> Integer -> Integer
getCostOfMovingCrabs initialPositions desiredPosition = foldr (\pos -> (+) (abs (desiredPosition - pos))) 0 initialPositions

getCostOfMovingCrabs' :: [Integer] -> Integer -> Integer
getCostOfMovingCrabs' initialPositions desiredPosition =
  foldr (\pos -> (+) ((triangular . abs) (desiredPosition - pos))) 0 initialPositions

triangular :: Integer -> Integer
triangular x = x * (x + 1) `div` 2

getInput :: FilePath -> IO [Integer]
getInput filePath = fmap (map (\s -> read s :: Integer) . splitOn "," . head . lines) (readFile filePath)