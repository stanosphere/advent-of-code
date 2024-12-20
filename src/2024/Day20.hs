module Day20 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (find, traverse_)
import Data.List (nub, partition)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import Utils.Grouping (frequencies, groupMapReduce)

type Coord = (Int, Int)

data Direction = U | D | L | R

data PathState = PS
  { _distance :: Int,
    _position :: Coord,
    _seen :: S.Set Coord -- could use a Maybe actually since it only ever has one value lol
  }

data CheatPathState = CPS
  { _cheatCost :: Int,
    _frontier :: [Coord],
    _seenWalls :: M.Map Coord Int
  }

-- actually we don't need dijkstra at all...
-- I think the maze might be just a simple path
-- that is each tile, I think, will have exactly two neighbours (other than start and end)
-- so I can naively mark the distances on those tiles
-- and then when we pass through a wall I can do some maths to work out how much time was saved, just a simple subtraction
-- that should do it...

part1 :: IO Int
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
nodeToNeighbours grid = filter (`S.member` grid) . nodeToNeighboursNoFilter

nodeToNeighboursNoFilter :: Coord -> [Coord]
nodeToNeighboursNoFilter (x, y) = [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

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

-- ok so part 2 is basically the same except at every position on the path we have many cheat options
-- I reckon for each place on the path I can work out the distinct positions I could walk to by cheating (going through walls)
-- and then I can use the same subtraction logic but the cheat cost will not be fixed at 2, it'll vary
-- in terms of how to find these cheats I think we just need to iterate going out manhattan-style (in fact I think there will be code for this from last year)

getCheats :: S.Set Coord -> Coord -> M.Map Coord Int
getCheats wallPositions wallStart =
  _seenWalls
    . fromJust
    . find ((== 20) . _cheatCost)
    . iterate evolve
    $ CPS 2 [wallStart] (M.singleton wallStart 2)
  where
    evolve :: CheatPathState -> CheatPathState
    evolve (CPS cheatCost front seen) = CPS cheatCost' front' seen'
      where
        cheatCost' = cheatCost + 1
        front' = concatMap (filter (`M.notMember` seen) . nodeToNeighboursNoFilter) front
        seen' = M.union seen (M.fromList . map (\x -> (x, cheatCost')) $ front')

getShortCutDistance' :: Int -> M.Map Coord Int -> Coord -> (Coord, Int) -> Maybe (Coord, Int)
getShortCutDistance' nonCheatDistance distanceMap beforePosition (afterPosition, cheatCost) = res
  where
    beforeDist = distanceMap M.! beforePosition
    afterDist = distanceMap M.! afterPosition

    -- not too sure about this inequality actually hmmm
    res =
      if afterDist > beforeDist
        then Just (afterPosition, (nonCheatDistance - afterDist) + beforeDist + cheatCost)
        else Nothing

-- 279644 is too low
part2 = do
  (walls, freeSpace, start, end) <- getInput
  let distanceMap = getAllDistances (S.fromList freeSpace) start end
  let noCheatDistance = distanceMap M.! end
  let wallPositions = S.fromList walls

  let pathPositions = M.keys distanceMap

  let res =
        length
          . M.filter (\x -> (noCheatDistance - x) >= 100)
          . groupMapReduce fst snd min
          . concatMap (getShortCutDistances' noCheatDistance distanceMap)
          $ pathPositions

  print res

-- CHEATS DONT JUST HAVE TO STAY IN WALLSSSSSSSSSSS

-- getShortCutDistances' :: Int -> M.Map Coord Int -> S.Set Coord -> Coord -> [Int]
getShortCutDistances' :: Int -> M.Map Coord Int -> Coord -> [((Coord, Coord), Int)]
getShortCutDistances' nonCheatDistance distanceMap beforePosition = blah
  where
    possibleEndPoints = generateAllManhattanPoints beforePosition 20
    actualEndpoints = filter (\(coord, _) -> coord `M.member` distanceMap) possibleEndPoints
    blah = map (\(end, score) -> ((beforePosition, end), score)) . mapMaybe (getShortCutDistance' nonCheatDistance distanceMap beforePosition) $ actualEndpoints

generateAllManhattanPoints :: Coord -> Int -> [(Coord, Int)]
generateAllManhattanPoints (x, y) d = concatMap (generateManhattanPoints (x, y)) [1 .. d]

-- https://stackoverflow.com/questions/75128474/how-to-generate-all-of-the-coordinates-that-are-within-a-manhattan-distance-r-of
generateManhattanPoints :: Coord -> Int -> [(Coord, Int)]
generateManhattanPoints (x, y) d =
  map (\c -> (c, d))
    . concatMap
      ( \offset ->
          let invOffset = d - offset
           in [ (x + offset, y + invOffset),
                (x + invOffset, y - offset),
                (x - offset, y - invOffset),
                (x - invOffset, y + offset)
              ]
      )
    $ [1 .. d]