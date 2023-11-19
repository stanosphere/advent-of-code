{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day3 where

import Data.Foldable (find)
import Data.List.Split (splitOn)
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
import Prelude hiding (id)

data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data Rect = Rect
  { id :: String,
    x0 :: Int,
    y0 :: Int,
    width :: Int,
    height :: Int
  }
  deriving (Show)

-- for each sqaure who has claimed it?
type Squares = M.Map Coords (S.Set String)

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
findClaimsWithNoOveralps :: Squares -> Maybe String
findClaimsWithNoOveralps =
  fmap fst
    . find (not . snd)
    . M.toList
    . foldl (M.unionWith (||)) M.empty
    . map setToMap
    . M.elems

setToMap :: S.Set String -> M.Map String Bool
setToMap set = M.fromSet (const (S.size set > 1)) set

countOverlaps :: Squares -> Int
countOverlaps = length . filter ((> 1) . S.size) . M.elems

aggregateOverlaps :: [Rect] -> Squares
aggregateOverlaps = foldl (M.unionWith S.union) M.empty . map toSquares

parseRect :: String -> Rect
parseRect s =
  let [id, rest] = splitOn " @ " s
      [xy, wh] = splitOn ": " rest
      [x0, y0] = splitOn "," xy
      [w, h] = splitOn "x" wh
   in Rect id (read x0) (read y0) (read x0 + read w - 1) (read y0 + read h - 1)

toSquares :: Rect -> Squares
toSquares (Rect id x0 y0 x1 y1) =
  M.fromList
    [ (Coords x y, S.singleton id)
      | x <- [x0 .. x1],
        y <- [y0 .. y1]
    ]
