module Day16PreProcessing where

import Data.Foldable (traverse_)
import Data.List
  ( find,
    isPrefixOf,
  )
import Data.List.Split (splitOn)
import Data.Map qualified as M
  ( Map,
    alter,
    empty,
    fromList,
    toList,
    (!),
  )
import Data.Maybe
  ( fromMaybe,
  )

data Node = Node
  { nodeId :: String,
    flowRate :: Int
  }

data Edge = Edge
  { fromId :: String,
    toId :: String
  }

instance Show Edge where
  show :: Edge -> String
  show e = "{ source:'" ++ fromId e ++ "', target:'" ++ toId e ++ "'},"

instance Show Node where
  show :: Node -> String
  show n =
    "{ id:'" ++ nodeId n ++ "', label:'" ++ (show . flowRate $ n) ++ "'},"

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input16.txt"
  let (nodes, edges) = parseLines input
  let (matrix, nodeMap) = graphToMatrix nodes edges
  let importantNodes = "AA" : (map nodeId . filter ((> 0) . flowRate) $ nodes)
  let x =
        map (\(a, b, c, d) -> (a ++ "," ++ b ++ "," ++ show c ++ "," ++ show d))
          . filter (\(a, b, c, d) -> d > 0)
          . map (\(a, b, c, d) -> (a, b, c, fromMaybe 0 $ d))
          . map
            ( \(i, j, v) ->
                (i, j, v, fmap flowRate . find ((== j) . nodeId) $ nodes)
            )
          . filter
            ( \(i, j, v) ->
                i `elem` importantNodes && j `elem` importantNodes && v > 0
            )
          . map (\(Indices i j, v) -> (nodeMap M.! i, nodeMap M.! j, v))
          . M.toList
          . getAllShortestPaths (length nodes)
          $ matrix
  print "HI"
  traverse_ print x

parseLines :: [String] -> ([Node], [Edge])
parseLines s =
  let parsed = map parseLine s
      nodes = map fst parsed
      edges = concatMap snd parsed
   in (nodes, edges)

parseLine :: String -> (Node, [Edge])
parseLine s =
  let [a, b] = splitOn ";" s
      nodeInfo = parseNodeInfo a
   in (nodeInfo, parseEdgeInfo nodeInfo b)

parseNodeInfo :: String -> Node
parseNodeInfo s =
  let [a, b] = splitOn " has flow rate=" s in Node (drop 6 a) (read b)

parseEdgeInfo :: Node -> String -> [Edge]
parseEdgeInfo (Node from _) s =
  let tos =
        if isPrefixOf " tunnels lead to valves " s
          then splitOn ", " . drop 24 $ s
          else splitOn ", " . drop 23 $ s
   in map (\to -> Edge from to) tos

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

-- matrix stuff below here

maxInt :: Int
maxInt = 1000000

data Indices = Indices
  { i :: Int,
    j :: Int
  }
  deriving (Eq, Ord, Show)

type Matrix = M.Map Indices Int

type NodeMap = M.Map Int String

graphToMatrix :: [Node] -> [Edge] -> (Matrix, NodeMap)
graphToMatrix nodes edges =
  let size = length nodes
      nodeMap = M.fromList . zip [0 ..] . map nodeId $ nodes
      something = [(Indices i j) | i <- [0 .. size - 1], j <- [0 .. size - 1]]
      x =
        map
          ( \(Indices i j) ->
              ( (Indices i j),
                find
                  ( \(Edge from to) ->
                      (from == (nodeMap M.! i)) && (to == (nodeMap M.! j))
                  )
                  edges
              )
          )
          something
      y =
        map
          ( \(ij, maybeEdge) ->
              (ij, maybe (if isDiagonal ij then 0 else maxInt) (\_ -> 1) maybeEdge)
          )
          x
      z = M.fromList y
   in (z, nodeMap)

isDiagonal :: Indices -> Bool
isDiagonal (Indices i j) = i == j

-- inspired by Introduction To Algorithms page 688
-- I think it's like Floyd-Warshall but not as good
extendShortestPaths :: Int -> Matrix -> Matrix -> Matrix
extendShortestPaths size l w =
  let res = do
        i <- [0 .. size - 1]
        j <- [0 .. size - 1]
        k <- [0 .. size - 1]
        return (i, j, k)
   in foldl
        (\l' -> \(i, j, k) -> M.alter (f (i, j, k) l w) (Indices i j) l')
        M.empty
        res
  where
    f :: (Int, Int, Int) -> Matrix -> Matrix -> Maybe Int -> Maybe Int
    f (i, j, k) l w Nothing =
      Just (min (l M.! (Indices i j)) (l M.! (Indices i k) + w M.! (Indices k j)))
    f (i, j, k) l w (Just l') =
      Just (min l' (l M.! (Indices i k) + w M.! (Indices k j)))

matrixFromList :: [[Int]] -> Matrix
matrixFromList xs =
  M.fromList
    [(Indices i j, v) | (i, r) <- zip [0 ..] xs, (j, v) <- zip [0 ..] r]

getAllShortestPaths :: Int -> Matrix -> Matrix
getAllShortestPaths size w =
  last . take (size - 1) . iterate (\l -> extendShortestPaths size l w) $ w

toyMatrix :: Matrix
toyMatrix =
  matrixFromList
    [ [0, 3, 8, maxInt, (-4)],
      [maxInt, 0, maxInt, 1, 7],
      [maxInt, 4, 0, maxInt, maxInt],
      [2, maxInt, (-5), 0, maxInt],
      [maxInt, maxInt, maxInt, 6, 0]
    ]

showMatrix :: Matrix -> Int -> IO ()
showMatrix m size =
  traverse_
    print
    [[m M.! (Indices i j) | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]
