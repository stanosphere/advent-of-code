module Day13 where

import Data.Foldable (traverse_)
import Data.List (nub)
import Data.List.Split (splitOn)

data Point = Point {x :: Int, y :: Int} deriving (Eq, Show)

type Paper = [Point]

data Instruction = FoldX Int | FoldY Int deriving (Show)

part1 :: IO ()
part1 = do
  (paper, instructions) <- realInput
  print . length . foldPaper (head instructions) $ paper

-- pop the result into excel and you get the answer of "EFLFJGRF"
part2 :: IO ()
part2 = do
  (paper, instructions) <- realInput
  let res = applyAllInstructions paper instructions
      xs = map x res
      ys = map ((* (-1)) . y) res
  traverse_ print xs
  print "HELLO"
  traverse_ print ys

applyAllInstructions :: Paper -> [Instruction] -> Paper
applyAllInstructions = foldl (flip foldPaper)

foldPaper :: Instruction -> Paper -> Paper
foldPaper (FoldX foldX) p =
  let pointsToBeFoldedLeft = filter ((> foldX) . x) p
      pointsToBeLeftAlone = filter ((< foldX) . x) p
   in nub (pointsToBeLeftAlone ++ map (\(Point x y) -> Point (2 * foldX - x) y) pointsToBeFoldedLeft)
foldPaper (FoldY foldY) p =
  let pointsToBeFoldedUp = filter ((> foldY) . y) p
      pointsToBeLeftAlone = filter ((< foldY) . y) p
   in nub (pointsToBeLeftAlone ++ map (\(Point x y) -> Point x (2 * foldY - y)) pointsToBeFoldedUp)

-- parsing stuff
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

realInput = do
  fullInput <- getLines "./fixtures/input13.txt"
  let paper = map (\[x, y] -> Point (read x) (read y)) . map (splitOn ",") . takeWhile (/= "") $ fullInput
  let instructions = map parseInstruction . map (drop 11) . drop 1 . dropWhile (/= "") $ fullInput
  return (paper, instructions)

parseInstruction :: String -> Instruction
parseInstruction s = case splitOn "=" s of
  ["x", x] -> FoldX (read x)
  ["y", y] -> FoldY (read y)

toyInputCoords :: [Point]
toyInputCoords =
  map
    (uncurry Point)
    [ (6, 10),
      (0, 14),
      (9, 10),
      (0, 3),
      (10, 4),
      (4, 11),
      (6, 0),
      (6, 12),
      (4, 1),
      (0, 13),
      (10, 12),
      (3, 4),
      (3, 0),
      (8, 4),
      (1, 10),
      (2, 14),
      (8, 10),
      (9, 0)
    ]