module Day4 where

import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List (sort)
import qualified Data.Map as M
import Data.Tuple (swap)
import Text.Parsec (between, char, digit, letter, many1, newline, parse, sepBy, (<|>))
import Text.ParserCombinators.Parsec (Parser)
import Utils.Grouping (frequencies)

data Room = Room
  { _encryptedName :: String,
    _sectorId :: Int,
    _checksum :: String
  }
  deriving (Show)

part1 :: IO Int
part1 = sum . map _sectorId . filter isValid <$> getInput

-- just manually search through when done
part2 :: IO ()
part2 = (>>= traverse_ print) $ map (\r -> (decrypt r, _sectorId r)) . filter isValid <$> getInput

decrypt :: Room -> String
decrypt (Room encryptedName sectorId _) = map (\x -> if x == '-' then ' ' else shiftChar sectorId x) encryptedName

shiftChar :: Int -> Char -> Char
shiftChar i x = intToChar M.! (((charToInt M.! x) + i) `mod` 26)
  where
    -- probably would be best to create the mapping only once at the start!
    mapping = zip [0 ..] ['a' .. 'z']
    intToChar = M.fromList mapping
    charToInt = M.fromList . map swap $ mapping

isValid :: Room -> Bool
isValid (Room encryptedName _ checksum) =
  (== checksum)
    . take 5
    . map snd
    . sort
    . map (first (* (-1)) . swap)
    . M.toList
    . frequencies
    . filter (/= '-')
    $ encryptedName

getInput :: IO [Room]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input4.txt"
  where
    inputParser = room `sepBy` newline

    room = Room <$> encryptedName <*> int <*> checksum

    encryptedName = many1 (letter <|> char '-')
    int = read <$> many1 digit
    checksum = between (char '[') (char ']') (many1 letter)

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res
