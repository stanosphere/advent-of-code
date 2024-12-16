module Day16UsingJunctionsAsNodes where

{- I decided to go with a simpler approach rather than finishing this one off! -}

import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

data Direction = N | E | S | W deriving (Show)

type Coord = (Int, Int)

data Junction = Junction {_location :: Coord, _direction :: Direction} deriving (Show)

-- might be able to just dijkstra this one
-- could maybe have a graph where the nodes are a combo of position and direction
-- and nodes only exist at junctions (places with 3 or more neighbours)
-- I don't yet see why this wouldn't work...

-- for each junction follow the path until you find another junction or a dead end
-- this will give you your edges which you could either work out dynamically or once at the start

--  (nodeId -> nodeId -> Int) -> -- scoreFn
--   (nodeId -> [nodeId]) -> -- neighbourGetter
--   (nodeId -> Bool) -> -- end node check
--   StartNode nodeId ->

-- scoreFn :: Junction -> Junction -> Int
-- scoreFn i j = 1

part1 = do
  js <- parseInput . lines <$> readFile "./fixtures/input16.txt"
  traverse_ print js
  print (length js)

toJunction :: S.Set Coord -> Coord -> Maybe [Junction]
toJunction points (x, y) = if length neighbours > 2 then Just (map (Junction (x, y)) neighbours) else Nothing
  where
    neighbours =
      mapMaybe
        (\(p, dir) -> if S.member p points then Just dir else Nothing)
        [((x - 1, y), W), ((x + 1, y), E), ((x, y - 1), N), ((x, y + 1), S)]

parseInput :: [String] -> [Junction]
parseInput xs = junctions
  where
    g = [(i, j) | (j, row) <- zip [0 ..] xs, (i, char) <- zip [0 ..] row, char `elem` "."]
    gSet = S.fromList g
    junctions = concat . mapMaybe (toJunction gSet) $ g