module Day12 where

import Data.Bifunctor (second)
import Data.Char (ord)
import Data.Foldable (traverse_)
import Data.Map qualified as M
  ( Map,
    filterWithKey,
    findWithDefault,
    fromList,
    lookup,
    toList,
  )
import Data.Maybe (mapMaybe)
import Data.Set qualified as S (Set, fromList)
import Utils.Dijkstra

data Node = Node {coords :: Coords, elevation :: Elevation}

type Edges = M.Map Coords [Coords]

type Elevation = Int

type Nodes = M.Map Coords Elevation

-- data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

-- data Node = Node {coords :: Coords, elevation :: Elevation}

-- type Elevation = Int

-- type Nodes = M.Map Coords Elevation

-- type Edges = M.Map Coords [Coords]

-- type StartNode = Coords

-- type EndNode = Coords

-- data Visitation = Visited | UnVisited deriving (Show, Eq, Ord)

-- type TentativeDistances = M.Map Coords (Int, Visitation)

-- 1.56 secs
-- answer: 380
part1 :: IO ()
part1 = solve startNodeSelector endNodeSelector edgeSelector
  where
    startNodeSelector = (== 'S')
    endNodeSelector = (== 'E')
    edgeSelector fromNode candidateToNode =
      if elevation candidateToNode - elevation fromNode <= 1
        then Just . coords $ candidateToNode
        else Nothing

-- 6.39 secs
part2 :: IO ()
part2 = solve startNodeSelector endNodeSelector edgeSelector
  where
    startNodeSelector = (== 'E')
    endNodeSelector = (`elem` "aS")
    edgeSelector fromNode candidateToNode =
      if elevation fromNode - elevation candidateToNode <= 1
        then Just . coords $ candidateToNode
        else Nothing

solve ::
  (Char -> Bool) -> (Char -> Bool) -> (Node -> Node -> Maybe Coords) -> IO ()
solve startNodeSelector endNodeSelector edgeSelector = do
  input <- getLines "./fixtures/input12.txt"
  let (nodes, startNode, endNodes) =
        getNodes startNodeSelector endNodeSelector input
  let edges = getEdges edgeSelector nodes
  let neighbourGetter n = M.findWithDefault [] n edges

  -- could probably use `find` here lol
  let res =
        M.filterWithKey (\k (_, v) -> k `elem` endNodes && v == Visited)
          . head
          . dropWhile (not . shouldStop endNodes)
          . dijkstra (const 1) neighbourGetter
          $ startNode
  traverse_ print . M.toList $ res

-- so much parsing stuff!
getEdges :: (Node -> Node -> Maybe Coords) -> Nodes -> Edges
getEdges shouldKeepEdge nds =
  M.fromList
    . map
      (mkEdgesForSingleNode shouldKeepEdge . getAdjacent nds . uncurry Node)
    . M.toList
    $ nds

mkEdgesForSingleNode :: (Node -> Node -> Maybe Coords) -> (Node, [Node]) -> (Coords, [Coords])
mkEdgesForSingleNode shouldKeepEdge (fromNode, candidateTos) =
  (coords fromNode, mapMaybe (shouldKeepEdge fromNode) candidateTos)

getAdjacent :: Nodes -> Node -> (Node, [Node])
getAdjacent nodeMap nd =
  let ndCoords = coords nd
      coordsToCheck =
        [ ndCoords {x = 1 + x ndCoords},
          ndCoords {x = (-1) + x ndCoords},
          ndCoords {y = 1 + y ndCoords},
          ndCoords {y = (-1) + y ndCoords}
        ]
   in ( nd,
        mapMaybe
          (\coords' -> fmap (Node coords') (M.lookup coords' nodeMap))
          coordsToCheck
      )

getNodes ::
  (Char -> Bool) ->
  (Char -> Bool) ->
  [String] ->
  (Nodes, StartNode, S.Set EndNode)
getNodes startNodeSelector endNodeSelector input =
  let nodes =
        [ (Coords i j, x)
          | (j, row) <- zip [0 ..] input,
            (i, x) <- zip [0 ..] row
        ]
      startNode = fst . head . filter (startNodeSelector . snd) $ nodes
      endNodes = S.fromList . map fst . filter (endNodeSelector . snd) $ nodes
      nodeMap = M.fromList . map (second (ord . adjustLetter)) $ nodes
   in (nodeMap, startNode, endNodes)
  where
    adjustLetter x
      | x == 'S' = 'a'
      | x == 'E' = 'z'
      | otherwise = x

-- boilerplatey things
toyInput :: [String]
toyInput = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
