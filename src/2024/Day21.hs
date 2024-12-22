module Day21 where

import Data.Char (isDigit)
import Data.Foldable (find)
import Data.List (permutations)
import Data.List.Extra (nubOrd)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Utils.Grouping (windows)

type Coord = (Int, Int)

type PathMap = M.Map (Char, Char) [String]

part1 = sum . map (\x -> shortestSequence x * numericPart x) <$> getInput

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input21.txt"

numericPart :: String -> Int
numericPart = read . filter isDigit

shortestSequence :: String -> Int
shortestSequence =
  minimum
    . map length
    . concatMap (f movesForDirPair)
    . concatMap (f movesForDirPair)
    . f movesForNumPair
  where
    f moves x = allChoices . map moves . pairs $ ('A' : x)
    (numPaths, dirPaths) = generateAllPaths
    movesForNumPair (from, to) = numPaths M.! (from, to)
    movesForDirPair (from, to) = dirPaths M.! (from, to)

allChoices :: [[[a]]] -> [[a]]
allChoices = map concat . foldr f [[]]
  where
    f :: [a] -> [[a]] -> [[a]]
    f elems acc = [x : y | x <- elems, y <- acc]

generateAllPaths :: (PathMap, PathMap)
generateAllPaths = (numPaths, dirPaths)
  where
    numPaths = M.fromList [((from, to), getPaths numPositions from to) | from <- map fst numPositions, to <- map fst numPositions]
    dirPaths = M.fromList [((from, to), getPaths dirPositions from to) | from <- map fst dirPositions, to <- map fst dirPositions]

    numPositions :: [(Char, Coord)]
    numPositions =
      [ ('7', (0, 0)),
        ('8', (1, 0)),
        ('9', (2, 0)),
        ('4', (0, 1)),
        ('5', (1, 1)),
        ('6', (2, 1)),
        ('1', (0, 2)),
        ('2', (1, 2)),
        ('3', (2, 2)),
        ('0', (1, 3)),
        ('A', (2, 3))
      ]

    dirPositions :: [(Char, Coord)]
    dirPositions =
      [ ('^', (1, 0)),
        ('A', (2, 0)),
        ('<', (0, 1)),
        ('v', (1, 1)),
        ('>', (2, 1))
      ]

    move :: Coord -> Char -> Coord
    move (x, y) '^' = (x, y - 1)
    move (x, y) 'v' = (x, y + 1)
    move (x, y) '>' = (x + 1, y)
    move (x, y) '<' = (x - 1, y)
    move _ c = error ("unexpected key: " ++ [c])

    getPaths :: [(Char, Coord)] -> Char -> Char -> [[Char]]
    getPaths positions from to = map (++ "A") . filter pathIsValid . nubOrd . permutations $ (lr ++ ud)
      where
        lr
          | fromX > toX = replicate (fromX - toX) '<'
          | fromX < toX = replicate (toX - fromX) '>'
          | otherwise = []
        ud
          | fromY > toY = replicate (fromY - toY) '^'
          | fromY < toY = replicate (toY - fromY) 'v'
          | otherwise = []
        (fromX, fromY) = getPosition from
        (toX, toY) = getPosition to
        getPosition char = snd . fromJust . find ((== char) . fst) $ positions
        pathIsValid = all (\c -> c `elem` map snd positions) . scanl move (fromX, fromY)

pairs :: [a] -> [(a, a)]
pairs = map toTuple . windows 2
  where
    toTuple [x, y] = (x, y)
    toTuple _ = undefined

-- might need this
-- hmmm but in each step I need to know which buttons were pressed I think
manhattanDist :: Coord -> Coord -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

{-
    +---+---+---+
    | 7 | 8 | 9 |
    +---+---+---+
    | 4 | 5 | 6 |
    +---+---+---+
    | 1 | 2 | 3 |
    +---+---+---+
        | 0 | A |
        +---+---+

        +---+---+
        | ^ | A |
    +---+---+---+
    | < | v | > |
    +---+---+---+
-}