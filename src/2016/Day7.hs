{-# LANGUAGE LambdaCase #-}

module Day7 where

import Data.Maybe (mapMaybe)
import Data.Tuple (swap)
import Text.Parsec as P (between, char, letter, many1, newline, parse, sepBy, (<|>))
import Text.ParserCombinators.Parsec (Parser)
import Utils.Grouping (windows)

data IPChunk = InBrackets String | OutOfBrackets String deriving (Show)

part1 :: IO Int
part1 = length . filter supportsTLS <$> getInput

part2 :: IO Int
part2 = length . filter supportsSSL <$> getInput

supportsSSL :: [IPChunk] -> Bool
supportsSSL xs =
  not . null $
    [ ()
      | x <- concatMap getCandidates ins,
        y <- concatMap getCandidates outs,
        x == swap y
    ]
  where
    (ins, outs) = partition' xs
    getCandidates =
      mapMaybe
        (\case [x, y, z] -> if x == z && x /= y then Just (x, y) else Nothing; _ -> Nothing)
        . windows 3

supportsTLS :: [IPChunk] -> Bool
supportsTLS xs = any containsAbba outs && (not . any containsAbba $ ins)
  where
    (ins, outs) = partition' xs
    containsAbba :: String -> Bool
    containsAbba =
      any
        (\case [w, x, y, z] -> w == z && x == y && w /= x; _ -> False)
        . windows 4

partition' :: [IPChunk] -> ([String], [String])
partition' =
  foldr
    ( \x (i, o) ->
        case x of
          (InBrackets s) -> (s : i, o)
          (OutOfBrackets s) -> (i, s : o)
    )
    ([], [])

getInput :: IO [[IPChunk]]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input7.txt"

inputParser :: Parser [[IPChunk]]
inputParser = many1 (inBrackets <|> outOfBrackets) `sepBy` newline
  where
    inBrackets :: Parser IPChunk
    inBrackets = InBrackets <$> between (char '[') (char ']') (many1 letter)

    outOfBrackets :: Parser IPChunk
    outOfBrackets = OutOfBrackets <$> many1 letter

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res
