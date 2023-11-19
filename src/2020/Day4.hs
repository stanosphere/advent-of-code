{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day4 where

import Data.List.Split (splitOn)
import Data.Map
  ( Map,
    fromList,
    keys,
    (!),
  )
import Text.Read (readMaybe)

type Passport = Map String String

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input4.txt"
  let passports = map passportToMap . splitIntoPassports $ contents
  let res = length . filter hasRequiredFields $ passports
  print res

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input4.txt"
  let passports = map passportToMap . splitIntoPassports $ contents
  let res = length . filter isValid $ passports
  print res

splitIntoPassports :: [String] -> [String]
splitIntoPassports = map unwords . splitOn [""]

passportToMap :: String -> Passport
passportToMap = fromList . map ((\[k, v] -> (k, v)) . splitOn ":") . splitOn " "

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

hasRequiredFields :: Passport -> Bool
hasRequiredFields passport =
  let requiredKeys =
        [ "byr", -- (Birth Year)
          "iyr", -- (Issue Year)
          "eyr", -- (Expiration Year)
          "hgt", -- (Height)
          "hcl", -- (Hair Color)
          "ecl", -- (Eye Color)
          "pid" -- (Passport ID)
          -- "cid" -- (Country ID)
        ]
      actualKeys = keys passport
   in all (`elem` actualKeys) requiredKeys

isValid :: Passport -> Bool
isValid passport =
  and
    [ hasRequiredFields passport,
      isValidBirthYear,
      isValidIssueYear,
      isValidExpirationYear,
      isValidHeight,
      isValidHairColor,
      isValidEyeColor,
      isValidPassportID
    ]
  where
    isValidBirthYear :: Bool
    isValidBirthYear = case readMaybeInt (passport ! "byr") of
      Just year -> year >= 1920 && year <= 2002
      Nothing -> False

    isValidIssueYear :: Bool
    isValidIssueYear = case readMaybeInt (passport ! "iyr") of
      Just year -> year >= 2010 && year <= 2020
      Nothing -> False

    isValidExpirationYear :: Bool
    isValidExpirationYear = case readMaybeInt (passport ! "eyr") of
      Just year -> year >= 2020 && year <= 2030
      Nothing -> False

    isValidHeight :: Bool
    isValidHeight =
      let height = passport ! "hgt"
          units = takeRight 2 height
          maybeValue = readMaybeInt . dropRight 2 $ height
       in case (units, maybeValue) of
            ("cm", Just value) -> value >= 150 && value <= 193
            ("in", Just value) -> value >= 59 && value <= 76
            _ -> False

    isValidHairColor :: Bool
    isValidHairColor =
      let hairColor = passport ! "hcl"
          t = tail hairColor
       in head hairColor == '#' && length t == 6 && all (`elem` "0123456789abcdef") t

    isValidEyeColor :: Bool
    isValidEyeColor = passport ! "ecl" `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    isValidPassportID :: Bool
    isValidPassportID =
      let pid = passport ! "pid"
       in length pid == 9 && all (`elem` "0123456789") pid

    readMaybeInt :: String -> Maybe Int
    readMaybeInt = readMaybe

    takeRight :: Int -> [a] -> [a]
    takeRight n xs = drop (length xs - n) xs

    dropRight :: Int -> [a] -> [a]
    dropRight n xs = take (length xs - n) xs

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.
-- iyr (Issue Year) - four digits; at least 2010 and at most 2020.
-- eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
-- hgt (Height) - a number followed by either cm or in:
---- If cm, the number must be at least 150 and at most 193.
---- If in, the number must be at least 59 and at most 76.
-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
-- pid (Passport ID) - a nine-digit number, including leading zeroes.