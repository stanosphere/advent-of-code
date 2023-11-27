module Day10 where

import Data.Foldable (find, traverse_)

data Posistion = Pos {x :: Int, y :: Int} deriving (Show, Eq)

data Velocity = Vel {vx :: Int, vy :: Int} deriving (Show)

type State = [(Posistion, Velocity)]

data GridSize = GS
  { minX :: Int,
    minY :: Int,
    maxX :: Int,
    maxY :: Int
  }
  deriving (Show)

-- point where grid area is a tiny 549 which is 61 * 9
-- so I solved this with a silly heuristic but if it works it works
-- good thing there wasn't any noise or like outliers lol
part1 :: IO ()
part1 = do
  inp <- fmap (map parseLine) . getLines $ "./fixtures/input10.txt"
  let states = take 30000 . iterate step $ inp
  let positions = map (map fst) states
  let candidate = head . filter (\x -> getGridArea x == 549) $ positions
  showPositions candidate

-- point where grid area is a tiny 549 which is 61 * 9
part2 :: IO ()
part2 = do
  inp <- fmap (map parseLine) . getLines $ "./fixtures/input10.txt"
  let states = take 30000 . iterate step $ inp
  let positions = zip [(0 :: Int) ..] (map (map fst) states)
  let candidate = find (\(_, x) -> getGridArea x == 549) $ positions
  print candidate

step :: State -> State
step = map stepPoint
  where
    stepPoint :: (Posistion, Velocity) -> (Posistion, Velocity)
    stepPoint (Pos x y, Vel vx vy) = (Pos (x + vx) (y + vy), Vel vx vy)

showState :: State -> IO ()
showState = showPositions . map fst

showPositions :: [Posistion] -> IO ()
showPositions ps =
  let (GS minX minY maxX maxY) = getGridSize ps
   in traverse_ (print . (\cy -> map (\cx -> if Pos cx cy `elem` ps then '#' else ' ') [minX .. maxX])) [minY .. maxY]

getGridSize :: [Posistion] -> GridSize
getGridSize =
  foldl
    (\(GS minX minY maxX maxY) (Pos x y) -> GS (min x minX) (min y minY) (max x maxX) (max y maxY))
    (GS maxBound maxBound minBound minBound)

getGridArea :: [Posistion] -> Int
getGridArea ps =
  let (GS minX minY maxX maxY) = getGridSize ps
   in (maxX - minX) * (maxY - minY)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseLine :: String -> (Posistion, Velocity)
parseLine line =
  let x = read . take 6 . drop 10 $ line
      y = read . take 6 . drop 18 $ line
      vx = read . take 2 . drop 36 $ line
      vy = read . take 2 . drop 40 $ line
   in (Pos x y, Vel vx vy)