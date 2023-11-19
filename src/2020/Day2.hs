module Day2 where

import Data.List ()
import Prelude hiding
  ( max,
    min,
  )

data PasswordInfo = PasswordInfo
  { letter :: Char,
    min :: Int,
    max :: Int,
    password :: String
  }
  deriving (Show)

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input2.txt"
  print . count passwordIsValid . map parseLine $ contents

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input2.txt"
  print . count passwordIsValid' . map parseLine $ contents

parseLine :: String -> PasswordInfo
parseLine s =
  let min = read . takeWhile (/= '-') $ s
      x1 = drop 1 . dropWhile (/= '-') $ s
      max = read . takeWhile (/= ' ') $ x1
      x2 = drop 1 . dropWhile (/= ' ') $ x1
      char = head x2
      pwd = drop 3 x2
   in PasswordInfo char min max pwd

passwordIsValid :: PasswordInfo -> Bool
passwordIsValid pwdInfo =
  let occurrences = count (letter pwdInfo ==) (password pwdInfo)
   in min pwdInfo <= occurrences && occurrences <= max pwdInfo

passwordIsValid' :: PasswordInfo -> Bool
passwordIsValid' pwdInfo =
  let x1 = password pwdInfo !! (min pwdInfo - 1)
      x2 = password pwdInfo !! (max pwdInfo - 1)
   in x1 /= x2 && (x1 == letter pwdInfo || x2 == letter pwdInfo)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
