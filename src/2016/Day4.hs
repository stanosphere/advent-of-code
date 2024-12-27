module Day4 where

import Data.Bifunctor (first)
import Data.List (sort)
import qualified Data.Map as M
import Data.Tuple (swap)
import Text.Parsec (between, char, digit, letter, many1, newline, parse, sepBy)
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

isValid :: Room -> Bool
isValid (Room encryptedName _ checksum) =
  (== checksum)
    . take 5
    . map snd
    . sort
    . map (first (* (-1)) . swap)
    . M.toList
    . frequencies
    $ encryptedName

getInput :: IO [Room]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input4.txt"
  where
    inputParser = room `sepBy` newline

    room = Room <$> encryptedName <*> int <*> checksum

    encryptedName = concat <$> many1 (many1 letter <* char '-')
    int = read <$> many1 digit
    checksum = between (char '[') (char ']') (many1 letter)

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res
