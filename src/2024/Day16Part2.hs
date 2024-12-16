module Day16Part2 where

import Data.List (nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Utils.Dijkstra (DijkstraState (visited), dijkstraVisitAll)
import Prelude hiding (flip)

data Direction = N | E | S | W deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data Node = Nd {_position :: Coord, _orientation :: Direction} deriving (Eq, Show, Ord)

bestPathScore = 102488

-- using this approach for part 2 https://www.reddit.com/r/adventofcode/comments/1hfboft/comment/m2bae5n/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
part2 :: IO ()
part2 = do
  (coords, start, end) <- parseInput . lines <$> readFile "./fixtures/input16.txt"
  let startNode = Nd start E
  let res = visited . unsafeGet . solve coords $ startNode
  let res1 = visited . unsafeGet . solve coords $ Nd end N
  let res2 = visited . unsafeGet . solve coords $ Nd end E
  let res3 = visited . unsafeGet . solve coords $ Nd end S
  let res4 = visited . unsafeGet . solve coords $ Nd end W

  let allNodes = coordsToNodes coords

  let res1' = filter (\(Nd pos dir) -> bestPathScore == (res M.! Nd pos dir + res1 M.! Nd pos (flip dir))) allNodes
  let res2' = filter (\(Nd pos dir) -> bestPathScore == (res M.! Nd pos dir + res2 M.! Nd pos (flip dir))) allNodes
  let res3' = filter (\(Nd pos dir) -> bestPathScore == (res M.! Nd pos dir + res3 M.! Nd pos (flip dir))) allNodes
  let res4' = filter (\(Nd pos dir) -> bestPathScore == (res M.! Nd pos dir + res4 M.! Nd pos (flip dir))) allNodes

  print . length . nub . map _position $ (res1' ++ res2' ++ res3' ++ res4')

unsafeGet :: Maybe a -> a
unsafeGet Nothing = error "oops"
unsafeGet (Just x) = x

solve :: [Coord] -> Node -> Maybe (DijkstraState Node)
solve coords = dijkstraVisitAll scoreFn neighbourGetter
  where
    scoreFn :: Node -> Node -> Int
    scoreFn x y = scoreMap M.! (x, y)

    neighbourGetter :: Node -> [Node]
    neighbourGetter n = neighbourMap M.! n

    coordSet = S.fromList coords
    neighbourMap =
      M.fromList
        . map (\n -> (n, map fst . nodeToNeighbours coordSet $ n))
        . coordsToNodes
        $ coords
    scoreMap =
      M.fromList
        . concatMap (\x -> map (\(y, score) -> ((x, y), score)) . nodeToNeighbours coordSet $ x)
        . coordsToNodes
        $ coords

coordsToNodes :: [Coord] -> [Node]
coordsToNodes = concatMap (\c -> map (Nd c) [N, E, S, W])

nodeToNeighbours :: S.Set Coord -> Node -> [(Node, Int)]
nodeToNeighbours s nd = (map (\x -> (x, 1000)) . otherOrientations $ nd) ++ (map (\x -> (x, 1)) . adjacent $ nd)
  where
    otherOrientations :: Node -> [Node]
    otherOrientations (Nd (x, y) N) = map (Nd (x, y)) [E, S, W]
    otherOrientations (Nd (x, y) E) = map (Nd (x, y)) [N, S, W]
    otherOrientations (Nd (x, y) S) = map (Nd (x, y)) [N, E, W]
    otherOrientations (Nd (x, y) W) = map (Nd (x, y)) [N, E, S]

    adjacent :: Node -> [Node]
    adjacent (Nd (x, y) N) = [Nd (x, y - 1) N | S.member (x, y - 1) s]
    adjacent (Nd (x, y) E) = [Nd (x + 1, y) E | S.member (x + 1, y) s]
    adjacent (Nd (x, y) S) = [Nd (x, y + 1) S | S.member (x, y + 1) s]
    adjacent (Nd (x, y) W) = [Nd (x - 1, y) W | S.member (x - 1, y) s]

flip :: Direction -> Direction
flip N = S
flip E = W
flip S = N
flip W = E

parseInput :: [String] -> ([Coord], Coord, Coord)
parseInput xs = (map fst g, start, end)
  where
    g = [((i, j), char) | (j, row) <- zip [0 ..] xs, (i, char) <- zip [0 ..] row, char `elem` "ES."]
    start = fst . head . filter ((== 'S') . snd) $ g
    end = fst . head . filter ((== 'E') . snd) $ g
