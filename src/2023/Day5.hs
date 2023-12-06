{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{-# HLINT ignore "Use <$>" #-}

module Day5 where

import Control.Monad (join)
import Data.Foldable (find)
import Data.Maybe (fromMaybe, isJust)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Mapping = Mapping {from :: String, to :: String, mapping :: Int -> Int} deriving (Show)

data Input = Input {seeds :: [Int], mappings :: [Mapping]} deriving (Show)

part1 :: IO Int
part1 = do
  rawInput <- readFile "./fixtures/input5.txt"
  let (Input seeds mappings) = unsafeParse inputParser rawInput
  return . minimum . map ((snd . get) . (\seed -> getLocation mappings ("seed", seed))) $ seeds

getLocation :: [Mapping] -> (String, Int) -> Maybe (String, Int)
getLocation xs = find ((== "location") . fst) . iterate (getNext xs)
  where
    getNext :: [Mapping] -> (String, Int) -> (String, Int)
    getNext mappings (from', value) =
      let myMapping = get . find (\x -> from x == from') $ mappings
       in (to myMapping, mapping myMapping value)

get :: Maybe a -> a
get Nothing = undefined
get (Just x) = x

instance Show (Int -> Int) where
  show :: (Int -> Int) -> String
  show _ = "Int -> Int"

inputParser :: Parser Input
inputParser = do
  seeds <- seedsParser
  P.newline
  mappings <- P.endBy mappingParser P.spaces
  return (Input seeds mappings)

seedsParser :: Parser [Int]
seedsParser = do
  P.string "seeds: "
  P.endBy intParser P.space

mappingParser :: Parser Mapping
mappingParser = do
  (from, to) <- headerParser
  ranges <- P.endBy rangeParser P.newline
  return (Mapping from to (rangesToFunction ranges))

headerParser :: Parser (String, String)
headerParser = do
  from <- P.many P.letter
  P.string "-to-"
  to <- P.many P.letter
  P.string " map:"
  P.spaces
  return (from, to)

data Range = Range {destStart :: Int, sourceStart :: Int, rangeLen :: Int} deriving (Show)

rangesToFunction :: [Range] -> Int -> Int
rangesToFunction = combineRangeFunctions . map rangeToFunction
  where
    rangeToFunction :: Range -> Int -> Maybe Int
    rangeToFunction (Range destStart sourceStart rangeLen) i =
      if i >= sourceStart && i < (sourceStart + rangeLen)
        then Just (destStart + (i - sourceStart))
        else Nothing
    combineRangeFunctions :: [Int -> Maybe Int] -> (Int -> Int)
    combineRangeFunctions fs i = fromMaybe i . join . find isJust . map (\f -> f i) $ fs

rangeParser :: Parser Range
rangeParser = do
  destStart <- intParser
  P.space
  sourceStart <- intParser
  P.space
  rangeLen <- intParser
  return (Range destStart sourceStart rangeLen)

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res