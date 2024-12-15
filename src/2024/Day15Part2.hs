module Day15Part2 where

import Data.Foldable (traverse_)
import qualified Data.Map as M

-- when moving up or down we may end up with a binary tree of boxes
-- if any path in this tree ends at a wall then no boxes in that path can move
-- so I think we just work out all the paths
-- work out if each path ends in a wall
-- if it does move no boxes in that path,or any boxes actually!!!
-- yes there we are we have a binary tree of boxes

-- throughout we consider the boxes to exist at the LEFT coord

data Move = U | D | L | R deriving (Show, Eq)

data GridSquare = Wall | Box deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = M.Map Coord GridSquare

data BoxPath = BP {_boxes :: [Coord], _endsInWall :: Bool}

part2 = do
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
    toChar (x, y) = if (x, y) == robotPosition then '@' else toChar' (x, y)
    toChar' (x, y) = case (M.lookup (x, y) grid, M.lookup (x - 1, y) grid) of
      (Just Wall, _) -> '#'
      (Just Box, _) -> '['
      (_, Just Box) -> ']'
      _ -> '.'

processMoves :: Coord -> Grid -> [Move] -> [(Coord, Grid)]
processMoves startingPosition startingGrid =
  scanl (\(robotPosition, grid) move -> attemptMove move robotPosition grid) (startingPosition, startingGrid)

attemptMove :: Move -> Coord -> Grid -> (Coord, Grid)
attemptMove L c g = attemptMoveLeft c g
attemptMove R c g = attemptMoveRight c g
attemptMove U c g = attemptMoveUp c g
attemptMove D c g = attemptMoveDown c g

attemptMoveLeft :: Coord -> Grid -> (Coord, Grid)
attemptMoveLeft (x0, y0) grid =
  case (M.lookup (x0 - 1, y0) grid, M.lookup (x0 - 2, y0) grid) of
    (_, Just Box) -> processBoxesLeft (x0, y0) (x0 - 2, y0) grid
    (Just Wall, _) -> ((x0, y0), grid)
    (_, _) -> ((x0 - 1, y0), grid)
  where
    processBoxesLeft :: Coord -> Coord -> Grid -> (Coord, Grid)
    processBoxesLeft (rx, ry) boxCoords grid = case boxPath of
      (BP _ True) -> ((rx, ry), grid)
      (BP oldBoxPositions False) -> ((rx - 1, ry), foldl processOldBoxPosition grid oldBoxPositions)
      where
        boxPath = getBoxPathLeft boxCoords grid (BP [] False)
        processOldBoxPosition g (x1, y1) = M.delete (x1, y1) . M.insert (x1 - 1, y1) Box $ g

    getBoxPathLeft :: Coord -> Grid -> BoxPath -> BoxPath
    getBoxPathLeft (x, y) grid (BP acc _) =
      case (nextSquare', nextSquare'') of
        (_, Just Box) -> getBoxPathLeft (x - 2, y) grid (BP acc' False)
        (Nothing, _) -> BP acc' False
        (Just Wall, _) -> BP acc' True
        (Just Box, _) -> error "oops"
      where
        nextSquare' = M.lookup (x - 1, y) grid
        nextSquare'' = M.lookup (x - 2, y) grid
        acc' = (x, y) : acc

attemptMoveRight :: Coord -> Grid -> (Coord, Grid)
attemptMoveRight (x0, y0) grid =
  case M.lookup (x0 + 1, y0) grid of
    Nothing -> ((x0 + 1, y0), grid)
    Just Wall -> ((x0, y0), grid)
    Just Box -> processBoxesRight (x0, y0) (x0 + 1, y0) grid
  where
    processBoxesRight :: Coord -> Coord -> Grid -> (Coord, Grid)
    processBoxesRight (rx, ry) boxCoords grid = case boxPath of
      (BP _ True) -> ((rx, ry), grid)
      (BP oldBoxPositions False) -> ((rx + 1, ry), foldl processOldBoxPosition grid oldBoxPositions)
      where
        boxPath = getBoxPathRight boxCoords grid (BP [] False)
        processOldBoxPosition g (x1, y1) = M.delete (x1, y1) . M.insert (x1 + 1, y1) Box $ g

    getBoxPathRight :: Coord -> Grid -> BoxPath -> BoxPath
    getBoxPathRight (x, y) grid (BP acc _) =
      case nextSquare' of
        Nothing -> BP acc' False
        Just Wall -> BP acc' True
        Just Box -> getBoxPathRight (x + 2, y) grid (BP acc' False)
      where
        nextSquare' = M.lookup (x + 2, y) grid
        acc' = (x, y) : acc

attemptMoveDown :: Coord -> Grid -> (Coord, Grid)
attemptMoveDown (x0, y0) grid = case (M.lookup (x0, y0 + 1) grid, M.lookup (x0 - 1, y0 + 1) grid) of
  (_, Just Box) -> processBoxesDown (x0, y0) (x0 - 1, y0 + 1)
  (Just Box, _) -> processBoxesDown (x0, y0) (x0, y0 + 1)
  (Just Wall, _) -> ((x0, y0), grid)
  (_, _) -> ((x0, y0 + 1), grid)
  where
    processBoxesDown :: Coord -> Coord -> (Coord, Grid)
    processBoxesDown (rx, ry) boxCoords = case boxPath of
      (BP _ True) -> ((rx, ry), grid)
      (BP oldBoxPositions False) -> ((rx, ry + 1), foldl processOldBoxPosition grid oldBoxPositions)
      where
        boxPath = getBoxPathDown boxCoords (BP [] False)
        processOldBoxPosition g (x1, y1) = M.delete (x1, y1) . M.insert (x1, y1 + 1) Box $ g

    -- more of a tree I suppose
    getBoxPathDown :: Coord -> BoxPath -> BoxPath
    getBoxPathDown (x, y) (BP acc _) =
      case (nextSquare', nextSquare'', nextSquare''') of
        -- just free space
        (Nothing, Nothing, Nothing) -> BP acc' False
        (Just Wall, Nothing, Nothing) -> BP acc' False
        -- wall
        (_, Just Wall, _) -> BP acc' True
        (_, _, Just Wall) -> BP acc' True
        -- box directly above
        (_, Just Box, _) -> getBoxPathDown (x, y + 1) (BP acc' False)
        -- maybe boxes diagonally above
        (Just Box, _, Just Box) -> combineBoxPaths (getBoxPathDown (x - 1, y + 1) (BP acc' False)) (getBoxPathDown (x + 1, y + 1) (BP acc' False))
        (_, _, Just Box) -> getBoxPathDown (x + 1, y + 1) (BP acc' False)
        (Just Box, _, Nothing) -> getBoxPathDown (x - 1, y + 1) (BP acc' False)
      where
        nextSquare' = M.lookup (x - 1, y + 1) grid
        nextSquare'' = M.lookup (x, y + 1) grid
        nextSquare''' = M.lookup (x + 1, y + 1) grid
        acc' = (x, y) : acc

attemptMoveUp :: Coord -> Grid -> (Coord, Grid)
attemptMoveUp (x0, y0) grid = case (M.lookup (x0, y0 - 1) grid, M.lookup (x0 - 1, y0 - 1) grid) of
  (_, Just Box) -> processBoxesUp (x0, y0) (x0 - 1, y0 - 1) grid
  (Just Box, _) -> processBoxesUp (x0, y0) (x0, y0 - 1) grid
  (Just Wall, _) -> ((x0, y0), grid)
  (_, _) -> ((x0, y0 - 1), grid)
  where
    processBoxesUp :: Coord -> Coord -> Grid -> (Coord, Grid)
    processBoxesUp (rx, ry) boxCoords grid = case boxPath of
      (BP _ True) -> ((rx, ry), grid)
      (BP oldBoxPositions False) -> ((rx, ry - 1), foldl processOldBoxPosition grid oldBoxPositions)
      where
        boxPath = getBoxPathUp boxCoords (BP [] False)
        processOldBoxPosition g (x1, y1) = M.delete (x1, y1) . M.insert (x1, y1 - 1) Box $ g

    getBoxPathUp :: Coord -> BoxPath -> BoxPath
    getBoxPathUp (x, y) (BP acc _) =
      case (nextSquare', nextSquare'', nextSquare''') of
        -- just free space
        (Nothing, Nothing, Nothing) -> BP acc' False
        (Just Wall, Nothing, Nothing) -> BP acc' False
        -- wall
        (_, Just Wall, _) -> BP acc' True
        (_, _, Just Wall) -> BP acc' True
        -- box directly above
        (_, Just Box, _) -> getBoxPathUp (x, y - 1) (BP acc' False)
        -- maybe boxes diagonally above
        (Just Box, _, Just Box) -> combineBoxPaths (getBoxPathUp (x - 1, y - 1) (BP acc' False)) (getBoxPathUp (x + 1, y - 1) (BP acc' False))
        (_, _, Just Box) -> getBoxPathUp (x + 1, y - 1) (BP acc' False)
        (Just Box, _, Nothing) -> getBoxPathUp (x - 1, y - 1) (BP acc' False)
      where
        nextSquare' = M.lookup (x - 1, y - 1) grid
        nextSquare'' = M.lookup (x, y - 1) grid
        nextSquare''' = M.lookup (x + 1, y - 1) grid
        acc' = (x, y) : acc

combineBoxPaths :: BoxPath -> BoxPath -> BoxPath
combineBoxPaths (BP acc b) (BP acc' b') = BP (acc ++ acc') (b || b')

getInput :: IO (Grid, [Move], Coord)
getInput = parseInput . lines <$> readFile "./fixtures/input15.txt"

parseInput :: [String] -> (Grid, [Move], Coord)
parseInput xs =
  let g = takeWhile (\x -> (not . null $ x) && ((== '#') . head $ x)) xs
      g' = [((i, j), char) | (j, row) <- zip [0 ..] g, (i, char) <- zip [0 ..] row]
      (x0, y0) = fst . head . filter ((== '@') . snd) $ g'
      grid = M.fromList . concatMap parseGridSquare $ g'
      moves = concatMap (map parseMove) . dropWhile (\x -> null x || ((== '#') . head $ x)) $ xs
   in (grid, moves, (x0 * 2, y0))
  where
    parseMove :: Char -> Move
    parseMove '<' = L
    parseMove '>' = R
    parseMove '^' = U
    parseMove 'v' = D
    parseMove e = error ("unexpect char: " ++ [e])

    parseGridSquare :: (Coord, Char) -> [(Coord, GridSquare)]
    parseGridSquare ((x, y), '#') = [((2 * x, y), Wall), ((2 * x + 1, y), Wall)]
    parseGridSquare ((x, y), 'O') = [((2 * x, y), Box)]
    parseGridSquare _ = []
