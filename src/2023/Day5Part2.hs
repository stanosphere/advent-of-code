module Day5Part2 where

import Data.Foldable (find, traverse_)
import Data.List.Split (chunksOf)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

data RangeMap = RangeMap {offset :: Int, sourceInterval :: Interval} deriving (Show)

data Interval = Interval {start :: Int, end :: Int} deriving (Show)

data Mapping = Mapping {from :: String, to :: String, range :: [RangeMap]} deriving (Show)

data Input = Input {seedIntervals :: [Interval], mappings :: [Mapping]} deriving (Show)

get :: Maybe a -> a
get Nothing = undefined
get (Just x) = x

getMapping :: String -> [Mapping] -> [RangeMap]
getMapping from' = range . get . find (\mp -> from mp == from')

-- 60568880, 0.09 secs
part2 :: IO Int
part2 = do
  rawInput <- readFile "./fixtures/input5.txt"
  let (Input seeds mappings) = unsafeParse inputParser rawInput
  let mapIntervals' s = mapIntervals (getMapping s mappings)

  traverse_ print seeds
  traverse_ printMapping mappings

  return
    . minimum
    . map start
    . mapIntervals' "humidity"
    . mapIntervals' "temperature"
    . mapIntervals' "light"
    . mapIntervals' "water"
    . mapIntervals' "fertilizer"
    . mapIntervals' "soil"
    . mapIntervals' "seed"
    $ seeds

printMapping :: Mapping -> IO ()
printMapping (Mapping from to ranges) = print ("from: " ++ from ++ ", to: " ++ to) *> traverse_ print ranges

mapIntervals :: [RangeMap] -> [Interval] -> [Interval]
mapIntervals rm = concatMap (`mapInterval'` rm)

mapInterval' :: Interval -> [RangeMap] -> [Interval]
mapInterval' i rms =
  let (as, bs) = foldl mapIntervalFolder ([], [i]) rms
   in as ++ bs

-- idea here is that I want to apply my range mappings one by one
-- essentially I want to find any intersections and this is just my weird way of doing that
-- I work out intersection for particular RangeMap and apply it
-- and remember the unintersected bits and apply my next RangeMap to those guys in the next call
-- hve to remember to combine my mapped and unmapped bits at the end
mapIntervalFolder :: ([Interval], [Interval]) -> RangeMap -> ([Interval], [Interval])
mapIntervalFolder (mapped, unMapped) rm =
  let new = concatMap (`mapInterval` rm) unMapped
      newMapped = map (applyOffset (offset rm) . fst) . filter snd $ new
      newUnMapped = map fst . filter (not . snd) $ new
   in (mapped ++ newMapped, newUnMapped)

mapInterval :: Interval -> RangeMap -> [(Interval, Bool)]
mapInterval myInterval (RangeMap _ sourceInterval)
  | noIntersection = [(myInterval, False)]
  | myIntervalEntirelyInsideSourceInterval = [(myInterval, True)]
  | sourceIntervalEntirelyInsideMyInterval = [(Interval (start myInterval) (start sourceInterval - 1), False), (sourceInterval, True), (Interval (end sourceInterval + 1) (end myInterval), False)]
  | myIntervalIntersectsLeftOfSourceInterval = [(Interval (start myInterval) (start sourceInterval - 1), False), (Interval (start sourceInterval) (end myInterval), True)]
  | myIntervalIntersectRightOfSourceInterval = [(Interval (start myInterval) (end sourceInterval), True), (Interval (end sourceInterval + 1) (end myInterval), False)]
  | otherwise = undefined
  where
    noIntersection = start myInterval > end sourceInterval || start sourceInterval > end myInterval
    myIntervalEntirelyInsideSourceInterval = start myInterval >= start sourceInterval && end myInterval <= end sourceInterval
    sourceIntervalEntirelyInsideMyInterval = start sourceInterval > start myInterval && end sourceInterval < end myInterval
    myIntervalIntersectsLeftOfSourceInterval = start myInterval <= start sourceInterval && end myInterval <= end sourceInterval
    myIntervalIntersectRightOfSourceInterval = start myInterval >= start sourceInterval && end myInterval >= end sourceInterval

applyOffset :: Int -> Interval -> Interval
applyOffset offset (Interval start end) = Interval (start + offset) (end + offset)

-- just parsing below here
inputParser :: Parser Input
inputParser = do
  seeds <- seedsParser
  P.newline
  mappings <- P.endBy mappingParser P.spaces
  return (Input seeds mappings)

seedsParser :: Parser [Interval]
seedsParser = do
  P.string "seeds: "
  ints <- P.endBy intParser P.space
  return . map (\[a, b] -> Interval a (a + b - 1)) . chunksOf 2 $ ints

mappingParser :: Parser Mapping
mappingParser = do
  (from, to) <- headerParser
  ranges <- P.endBy rangeParser P.newline
  return (Mapping from to ranges)

headerParser :: Parser (String, String)
headerParser = do
  from <- P.many P.letter
  P.string "-to-"
  to <- P.many P.letter
  P.string " map:"
  P.spaces
  return (from, to)

rangeParser :: Parser RangeMap
rangeParser = do
  destStart <- intParser
  P.space
  sourceStart <- intParser
  P.space
  rangeLen <- intParser
  let destInterval = destStart - sourceStart
  let sourceInterval = Interval sourceStart (sourceStart + rangeLen - 1)
  return (RangeMap destInterval sourceInterval)

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res