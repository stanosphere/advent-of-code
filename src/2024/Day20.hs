module Day20 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (traverse_)
import Data.List (partition, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Utils.Dijkstra (DijkstraResult (FoundEndNode), dijkstra)
import Utils.Grouping (frequencies)

type Coord = (Int, Int)

data Direction = U | D | L | R

type Result = DijkstraResult Coord Int

-- think I'll just use my "standard" dijkstra
-- a modified version would be better though
-- even just not starting from scratch each time would be really helpful
-- which I don't think would be too difficult
-- but let's see how well standard dijkstra works first before trying anything too fancy

part1 = do
  (walls, freeSpace, start, end) <- getInput
  let noCheatShortestPath = solve freeSpace start end Nothing
  let cheatSols =
        sortOn (fst)
          . M.toList
          . frequencies
          . map (getTimeSaved noCheatShortestPath)
          . filter ((> 0) . getTimeSaved noCheatShortestPath)
          . map (solve freeSpace start end . Just)
          . findWallsThatCanBePassedThrough (S.fromList freeSpace)
          $ walls

  print noCheatShortestPath
  print "cheats"
  traverse_ print cheatSols

getTimeSaved :: Result -> Result -> Int
getTimeSaved (FoundEndNode (_, t0)) (FoundEndNode (_, t1)) = t0 - t1
getTimeSaved _ _ = error "unexpected result type!"

solve :: [Coord] -> Coord -> Coord -> Maybe (Coord, Direction) -> Result
solve coords start end cheatSpace = dijkstra neighbourGetter (== end) start
  where
    neighbourGetter :: Coord -> [(Coord, Int)]
    neighbourGetter n = neighbourMap M.! n

    coordSet = S.fromList coords

    neighbourMap =
      addCheatToNeighbourMap
        . M.fromList
        . map (\n -> (n, map (\x -> (x, 1)) . nodeToNeighbours coordSet $ n))
        $ coords

    addCheatToNeighbourMap :: M.Map Coord [(Coord, Int)] -> M.Map Coord [(Coord, Int)]
    addCheatToNeighbourMap nm = case cheatSpace of
      Nothing -> nm
      Just ((cx, cy), R) -> M.insert (cx, cy) [((cx + 1, cy), 1)] nm
      Just ((cx, cy), L) -> M.insert (cx, cy) [((cx - 1, cy), 1)] nm
      Just ((cx, cy), U) -> M.insert (cx, cy) [((cx, cy - 1), 1)] nm
      Just ((cx, cy), D) -> M.insert (cx, cy) [((cx, cy + 1), 1)] nm

    nodeToNeighbours :: S.Set Coord -> Coord -> [Coord]
    nodeToNeighbours grid (x, y) = filter (`S.member` grid) [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)] ++ cheat
      where
        cheat = case cheatSpace of
          Nothing -> []
          Just ((cx, cy), U) -> [(cx, cy) | (cx, cy + 1) == (x, y)]
          Just ((cx, cy), D) -> [(cx, cy) | (cx, cy - 1) == (x, y)]
          Just ((cx, cy), L) -> [(cx, cy) | (cx + 1, cy) == (x, y)]
          Just ((cx, cy), R) -> [(cx, cy) | (cx - 1, cy) == (x, y)]

findWallsThatCanBePassedThrough :: S.Set Coord -> [Coord] -> [(Coord, Direction)]
findWallsThatCanBePassedThrough freeSpaces = concatMap (toPassable freeSpaces)

toPassable :: S.Set Coord -> Coord -> [(Coord, Direction)]
toPassable freeSpaces wall = upDown ++ leftRight
  where
    upDown = if isPassableUpDown wall then [(wall, U), (wall, D)] else []
    leftRight = if isPassableLeftRight wall then [(wall, L), (wall, R)] else []

    isPassableUpDown :: Coord -> Bool
    isPassableUpDown (x, y) = (x, y + 1) `S.member` freeSpaces && (x, y - 1) `S.member` freeSpaces

    isPassableLeftRight :: Coord -> Bool
    isPassableLeftRight (x, y) = (x + 1, y) `S.member` freeSpaces && (x - 1, y) `S.member` freeSpaces

getInput :: IO ([Coord], [Coord], Coord, Coord)
getInput = parseInput . lines <$> readFile "./fixtures/input20.txt"

parseInput :: [String] -> ([Coord], [Coord], Coord, Coord)
parseInput xs = (walls, freeSpace, start, end)
  where
    grid = [((i, j), char) | (j, row) <- zip [0 ..] xs, (i, char) <- zip [0 ..] row]
    (walls, freeSpace) = mapBoth (map fst) . partition ((== '#') . snd) $ grid
    start = fst . head . filter ((== 'S') . snd) $ grid
    end = fst . head . filter ((== 'E') . snd) $ grid

mapBoth :: (Bifunctor bf) => (a -> b) -> bf a a -> bf b b
mapBoth f = bimap f f