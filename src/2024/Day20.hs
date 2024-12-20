module Day20 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (partition)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

type Coord = (Int, Int)

data Direction = U | D | L | R

data PathState = PS
  { _distance :: Int,
    _position :: Coord,
    _seen :: S.Set Coord -- could use a Maybe actually since it only ever has one value lol
  }

-- actually we don't need dijkstra at all...
-- I think the maze might be just a simple path
-- that is each tile, I think, will have exactly two neighbours (other than start and end)
-- so I can naively mark the distances on those tiles
-- and then when we pass through a wall I can do some maths to work out how much time was saved, just a simple subtraction
-- that should do it...

part1 = do
  (walls, freeSpace, start, end) <- getInput
  let distanceMap = getAllDistances (S.fromList freeSpace) start end
  let noCheatDistance = distanceMap M.! end
  return
    . length
    . filter (\x -> (noCheatDistance - x) >= 100)
    . mapMaybe (getShortCutDistance noCheatDistance distanceMap)
    . findWallsThatCanBePassedThrough (S.fromList freeSpace)
    $ walls

getShortCutDistance :: Int -> M.Map Coord Int -> (Coord, Direction) -> Maybe Int
getShortCutDistance nonCheatDistance distanceMap (wallPosition, direction) =
  if afterDist > beforeDist
    then Just ((nonCheatDistance - afterDist) + beforeDist + 2)
    else Nothing
  where
    (before, after) = case (wallPosition, direction) of
      ((x, y), U) -> ((x, y - 1), (x, y + 1))
      ((x, y), D) -> ((x, y + 1), (x, y - 1))
      ((x, y), L) -> ((x - 1, y), (x + 1, y))
      ((x, y), R) -> ((x + 1, y), (x - 1, y))
    beforeDist = distanceMap M.! before
    afterDist = distanceMap M.! after

getAllDistances :: S.Set Coord -> Coord -> Coord -> M.Map Coord Int
getAllDistances freeSpaces start end =
  M.fromList
    . map (\(PS distance position _) -> (position, distance))
    . takeWhileOneMore ((/= end) . _position)
    . iterate updatePathState
    $ PS 0 start S.empty
  where
    updatePathState :: PathState -> PathState
    updatePathState (PS distance position seen) = PS distance' position' seen'
      where
        distance' = distance + 1
        position' = head . filter (`S.notMember` seen) . nodeToNeighbours freeSpaces $ position
        seen' = S.singleton position

nodeToNeighbours :: S.Set Coord -> Coord -> [Coord]
nodeToNeighbours grid (x, y) = filter (`S.member` grid) [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

findWallsThatCanBePassedThrough :: S.Set Coord -> [Coord] -> [(Coord, Direction)]
findWallsThatCanBePassedThrough freeSpaces = concatMap toPassable
  where
    toPassable :: Coord -> [(Coord, Direction)]
    toPassable wall = upDown ++ leftRight
      where
        upDown = if isPassableUpDown wall then [(wall, U), (wall, D)] else []
        leftRight = if isPassableLeftRight wall then [(wall, L), (wall, R)] else []

        isPassableUpDown :: Coord -> Bool
        isPassableUpDown (x, y) = (x, y + 1) `S.member` freeSpaces && (x, y - 1) `S.member` freeSpaces

        isPassableLeftRight :: Coord -> Bool
        isPassableLeftRight (x, y) = (x + 1, y) `S.member` freeSpaces && (x - 1, y) `S.member` freeSpaces

mapBoth :: (Bifunctor bf) => (a -> b) -> bf a a -> bf b b
mapBoth f = bimap f f

-- from stack overflow
takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

getInput :: IO ([Coord], [Coord], Coord, Coord)
getInput = parseInput . lines <$> readFile "./fixtures/input20.txt"

parseInput :: [String] -> ([Coord], [Coord], Coord, Coord)
parseInput xs = (walls, freeSpace, start, end)
  where
    grid = [((i, j), char) | (j, row) <- zip [0 ..] xs, (i, char) <- zip [0 ..] row]
    (walls, freeSpace) = mapBoth (map fst) . partition ((== '#') . snd) $ grid
    start = fst . head . filter ((== 'S') . snd) $ grid
    end = fst . head . filter ((== 'E') . snd) $ grid
