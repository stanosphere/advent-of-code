-- I really should look up nice ways to do this on the reddit thread, or just think a bit harder!
module Day7 where

import Data.Foldable (find)
import Data.Function (on)
import Data.List
  ( groupBy,
    sortOn,
  )
import Data.Map qualified as M
  ( Map,
    delete,
    filter,
    fromList,
    fromSet,
    keys,
    map,
    null,
    union,
  )
import Data.Set qualified as S
  ( difference,
    fromList,
  )

data Edge = Edge {from :: Char, to :: Char} deriving (Show)

type TaskMap = M.Map Char [Char]

data State = State {taskMap :: TaskMap, stepOrder :: [Char]} deriving (Show)

-- gives answer of IJLFUVDACEHGRZPNKQWSBTMXOY in 0.03 seconds
part1 :: IO ()
part1 = do
  inp <- getLines "./fixtures/input7.txt"
  let initState = State (toTaskMap . map parseRelationship $ inp) []
  print . fmap (reverse . stepOrder) . find (M.null . taskMap) . iterate step $ initState

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseRelationship :: String -> Edge
parseRelationship s = Edge (s !! 5) (s !! 36)

toTaskMap :: [Edge] -> TaskMap
toTaskMap es = M.union canDoStraightAway cantDoYet
  where
    canDoStraightAway = groupMap to from es
    cantDoYet = getTasksWeCanDo es

getTasksWeCanDo :: [Edge] -> TaskMap
getTasksWeCanDo es = M.fromSet (const []) (S.difference froms tos)
  where
    froms = S.fromList . map from $ es
    tos = S.fromList . map to $ es

step :: State -> State
step (State taskMap stepOrder) = State taskMap' stepOrder'
  where
    nextTaskToDo = minimum . M.keys . M.filter null $ taskMap
    taskMap' = M.map (filter (/= nextTaskToDo)) . M.delete nextTaskToDo $ taskMap
    stepOrder' = nextTaskToDo : stepOrder

-- inspired by scala function of the same name
groupMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> M.Map k [v]
groupMap keyBy mapBy =
  M.fromList
    . map (\xs -> (keyBy . head $ xs, map mapBy xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy
