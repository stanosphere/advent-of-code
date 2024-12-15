module Day16 where

import Data.List (partition, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M (Map, (!))
import qualified Data.Set as S
  ( Set,
    empty,
    fromList,
    intersection,
    map,
    toList,
    unions,
  )
import Utils.Grouping (groupBy')

data Edge = Edge
  { from :: String,
    to :: String,
    distance :: Int,
    flowRate :: Int
  }
  deriving (Show, Eq, Ord)

-- remember each valve takes one minute to open
-- only then will the flow start

type EdgeMap = M.Map String [Edge]

-- NOTE THIS WON't WORK BECAUSE I'VE HARDCODED 26 INSTEAD OF 30 IN A FEW PLACES
part1 :: IO ()
part1 = do
  lines <- getLines "./fixtures/input16preProcessedSmall.txt"
  print
    . maximum
    . S.map scorePath
    . findPaths "AA"
    . groupBy' from
    . map parseLine
    $ lines

part2 :: IO ()
part2 = do
  lines <- getLines "./fixtures/input16preProcessed.txt"
  print
    . maximum
    . map (\((_, x), (_, y)) -> x + y)
    . getCandidatePairs
    . take 2000
    . sortOn ((* (-1)) . snd)
    . S.toList
    . S.map pathToSummary
    . findPaths "AA"
    . groupBy' from
    . map parseLine
    $ lines

getCandidatePairs :: [([String], Int)] -> [(([String], Int), ([String], Int))]
getCandidatePairs paths =
  filter
    (\((xs, _), (ys, _)) -> areDifferent (drop 1 xs) (drop 1 ys))
    ( do
        x <- paths
        y <- paths
        return (x, y)
    )

areDifferent :: [String] -> [String] -> Bool
areDifferent a b = S.empty == S.intersection (S.fromList a) (S.fromList b)

pathToSummary :: Path -> ([String], Int)
pathToSummary p =
  let relevantEdges = if isCyclic p then drop 1 . nodes $ p else nodes p
   in (map to . reverse $ relevantEdges, scorePath p)

data Acc = Acc
  { dist :: Int,
    totalFlow :: Int
  }

-- from to distance flowRate
-- look at tos
scorePath :: Path -> Int
scorePath (Path edges _) = scorePath' edges

scorePath' :: [Edge] -> Int
scorePath' = totalFlow . foldl f (Acc 0 0) . drop 1 . reverse
  where
    f :: Acc -> Edge -> Acc
    f (Acc totalDist totalFlow) (Edge _ _ distance flowRate) =
      let timeLeft = 26 - (totalDist + 1 + distance)
          newTotalFlow =
            if timeLeft > 0 then timeLeft * flowRate + totalFlow else totalFlow
       in Acc (totalDist + 1 + distance) newTotalFlow

parseLine :: String -> Edge
parseLine s =
  let [from, to, distance, flowRate] = splitOn "," s
   in Edge from to (read distance) (read flowRate)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

findPaths :: String -> EdgeMap -> TerminalPaths
findPaths startNode edgeMap =
  let startingEdge = Edge startNode startNode 0 0
      startingPaths = [Path [startingEdge] 0]
   in go edgeMap (S.empty, startingPaths)

-- head is most recent node visited
data Path = Path
  { nodes :: [Edge],
    score :: Int
  }
  deriving (Show, Eq, Ord)

type Paths = [Path]

type TerminalPaths = S.Set Path

go :: EdgeMap -> (TerminalPaths, Paths) -> TerminalPaths
go edgeMap (terminal, nonTerminal) =
  if null nonTerminal
    then terminal
    else
      let (newTps, newPaths) = unzip . map (addToPath edgeMap) $ nonTerminal
       in go
            edgeMap
            ( S.unions (terminal : newTps),
              filter (not . isCyclic) . concat $ newPaths
            )

addToPath :: EdgeMap -> Path -> (TerminalPaths, Paths)
addToPath edgeMap path =
  let latestNode = to . head . nodes $ path
      edges = edgeMap M.! latestNode
      (tps, ps) =
        partition (\p -> score p >= 26)
          . map (\e -> Path (e : nodes path) (score path + distance e + 1))
          $ edges
   in (S.fromList . map removeFinalEdge $ tps, ps)

-- because we don't need it
removeFinalEdge :: Path -> Path
removeFinalEdge (Path edges score) = Path (tail edges) score

-- have we seen the head node before?
isCyclic :: Path -> Bool
isCyclic (Path edges _) =
  let h = to . head $ edges in any (\e -> from e == h) . tail $ edges
