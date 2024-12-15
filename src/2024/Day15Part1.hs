module Day15Part1 where

import Data.Bifunctor (second)
import Data.Foldable (traverse_)
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)

data Move = U | D | L | R deriving (Show, Eq)

data GridSquare = Wall | Box | Free deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = M.Map Coord GridSquare

part1 :: IO ()
part1 = do
  (grid, moves, startingPosition) <- getInput
  let res = last . processMoves startingPosition grid $ moves
  let (_, finalGrid) = res
  let score = sum . map (\(x, y) -> x + 100 * y) . M.keys . M.filter (== Box) $ finalGrid
  display res
  print score

display :: (Coord, Grid) -> IO ()
display (robotPosition, grid) = traverse_ putStrLn [[toChar (x, y) | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    elems = M.keys grid
    maxX = maximum . map fst $ elems
    maxY = maximum . map snd $ elems
    toChar :: Coord -> Char
    toChar (x, y) =
      if (x, y) == robotPosition
        then '@'
        else case grid M.! (x, y) of
          Wall -> '#'
          Box -> 'O'
          Free -> '.'

processMoves :: Coord -> Grid -> [Move] -> [(Coord, Grid)]
processMoves startingPosition startingGrid =
  scanl (\(robotPosition, grid) move -> attemptMove move robotPosition grid) (startingPosition, startingGrid)

attemptMove :: Move -> Coord -> Grid -> (Coord, Grid)
attemptMove L = attemptMove' (\(x, y) -> (x - 1, y))
attemptMove R = attemptMove' (\(x, y) -> (x + 1, y))
attemptMove U = attemptMove' (\(x, y) -> (x, y - 1))
attemptMove D = attemptMove' (\(x, y) -> (x, y + 1))

-- case work
-- square is a wall: nothing happens
-- square is free space: move the robot
-- sequence of boxes against a wall: nothing happens
-- sequence of boxes and a free space to move them into:move the boxes

attemptMove' :: (Coord -> Coord) -> Coord -> Grid -> (Coord, Grid)
attemptMove' stepFn robotPosition grid =
  case head squaresInDirection of
    (_, Wall) -> (robotPosition, grid)
    ((x, y), Free) -> ((x, y), grid)
    (_, Box) -> processBoxes squaresInDirection robotPosition grid
  where
    squaresInDirection = drop 1 . catMaybes . takeWhile isJust . map (`lookup'` grid) . iterate stepFn $ robotPosition

processBoxes :: [(Coord, GridSquare)] -> Coord -> Grid -> (Coord, Grid)
processBoxes boxSquares robotPosition grid =
  if cantMove
    then (robotPosition, grid)
    else case find ((== Free) . snd) boxSquares of
      Nothing -> (robotPosition, grid)
      Just (firstFreePosition, _) -> processBoxes' (firstFreePosition, newRobotPosition) grid
  where
    cantMove = all (== Box) . takeWhile (/= Wall) . map snd $ boxSquares
    newRobotPosition = fst . head $ boxSquares

-- if we have a line of boxes then we only need to change two things in the grid
-- - square where the robot moves to is now free
-- - first free square we found now has a box in it
processBoxes' :: (Coord, Coord) -> Grid -> (Coord, Grid)
processBoxes' (firstFreePosition, newRobotPosition) grid =
  ( newRobotPosition,
    M.insert firstFreePosition Box . M.insert newRobotPosition Free $ grid
  )

-- have to move the robot one to the left
-- and then fill the squares up to and including first free space

lookup' :: (Ord k) => k -> M.Map k v -> Maybe (k, v)
lookup' key mp = case M.lookup key mp of Just val -> Just (key, val); Nothing -> Nothing

getInput :: IO (Grid, [Move], Coord)
getInput = parseInput . lines <$> readFile "./fixtures/input15.txt"

parseInput :: [String] -> (Grid, [Move], Coord)
parseInput xs =
  let g = takeWhile (\x -> (not . null $ x) && ((== '#') . head $ x)) xs
      g' = [((i, j), char) | (j, row) <- zip [0 ..] g, (i, char) <- zip [0 ..] row]
      robotPos = fst . head . filter ((== '@') . snd) $ g'
      grid = M.fromList . map (second parseGridSquare) $ g'
      moves = concatMap (map parseMove) . dropWhile (\x -> null x || ((== '#') . head $ x)) $ xs
   in (grid, moves, robotPos)
  where
    parseMove :: Char -> Move
    parseMove '<' = L
    parseMove '>' = R
    parseMove '^' = U
    parseMove 'v' = D
    parseMove e = error ("unexpect char: " ++ [e])

    parseGridSquare :: Char -> GridSquare
    parseGridSquare '#' = Wall
    parseGridSquare 'O' = Box
    parseGridSquare _ = Free
