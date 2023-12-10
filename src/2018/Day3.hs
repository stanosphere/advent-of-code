{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day3 where

import Data.Foldable (find)
import Data.Map qualified as M
  ( Map,
    elems,
    empty,
    fromList,
    fromSet,
    toList,
    unionWith,
  )
import Data.Set qualified as S
  ( Set,
    singleton,
    size,
    union,
  )
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data Rect = Rect
  { id :: Int,
    x0 :: Int,
    y0 :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

-- for each sqaure who has claimed it?
type Squares = M.Map Coords (S.Set Int)

part1 :: IO ()
part1 = do
  rawInput <- getLines "./fixtures/input3.txt"
  print . countOverlaps . aggregateOverlaps . map parseRect $ rawInput

part2 :: IO ()
part2 = do
  rawInput <- getLines "./fixtures/input3.txt"
  print . findClaimsWithNoOveralps . aggregateOverlaps . map parseRect $ rawInput

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

-- so what I have here is sets that tell me about which claims overlap
-- so if i deduplicate it and then make a list of stuff that never appears in sets larger than one
findClaimsWithNoOveralps :: Squares -> Maybe Int
findClaimsWithNoOveralps =
  fmap fst
    . find (not . snd)
    . M.toList
    . foldl (M.unionWith (||)) M.empty
    . map setToMap
    . M.elems

setToMap :: S.Set k -> M.Map k Bool
setToMap set = M.fromSet (const (S.size set > 1)) set

countOverlaps :: Squares -> Int
countOverlaps = length . filter ((> 1) . S.size) . M.elems

aggregateOverlaps :: [Rect] -> Squares
aggregateOverlaps = foldl (M.unionWith S.union) M.empty . map toSquares

parseRect :: String -> Rect
parseRect = unsafeParse rectParser

toSquares :: Rect -> Squares
toSquares (Rect rectId x0 y0 x1 y1) =
  M.fromList
    [ (Coords x y, S.singleton rectId)
      | x <- [x0 .. x1],
        y <- [y0 .. y1]
    ]

-- Parsing stuff below here
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "still don't really know wht this arg is for lol" s of
  Left res -> error . show $ res
  Right res -> res

rectParser :: Parser Rect
rectParser = do
  P.char '#'
  rectId <- intParser
  P.string " @ "
  x0 <- intParser
  P.char ','
  y0 <- intParser
  P.string ": "
  w <- intParser
  let x1 = x0 + w - 1
  P.char 'x'
  h <- intParser
  let y1 = y0 + h - 1
  return (Rect rectId x0 y0 x1 y1)

intParser :: Parser Int
intParser = read <$> P.many1 P.digit