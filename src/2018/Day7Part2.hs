module Day7Part2 where

import Data.Foldable (find, traverse_)
import Data.Function (on)
import Data.List
  ( groupBy,
    sort,
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
    size,
    union,
    (!),
  )
import Data.Set qualified as S
  ( difference,
    fromList,
  )

data Edge = Edge {from :: Char, to :: Char} deriving (Show)

type TaskMap = M.Map Char [Char]

data StepWorkedOn = StepWorkedOn {stepId :: Char, timeLeft :: Int} deriving (Show)

data State = State
  { time :: Int,
    taskMap :: TaskMap,
    stepsWorkedOn :: [StepWorkedOn],
    stepOrder :: [Char]
  }
  deriving (Show)

-- 1072
go :: IO ()
go = do
  inp <- getLines "./fixtures/input7.txt"
  let initState = State 0 (toTaskMap . map parseRelationship $ inp) [] []
  let xs = take 1100 . iterate step $ initState
  traverse_ prettyPrint xs
  where
    prettyPrint :: State -> IO ()
    prettyPrint s = (print "---------------") *> (print . time $ s) *> (print . stepOrder $ s) *> (print . stepsWorkedOn $ s)

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
step (State time taskMap stepsWorkedOn stepOrder) =
  let time' = time + 1
      advancedTasks = advanceTasks 1 stepsWorkedOn
      finishedTasks = findFinishedTasks advancedTasks
      ongoingTasks = removeFinishedTasks advancedTasks
      taskMap' = handleFinishedTasks finishedTasks taskMap
      taskToStart = getTasksToStart ongoingTasks taskMap'
      stepsWorkedOn' = startTasks taskToStart ongoingTasks
      stepOrder' = stepOrder ++ finishedTasks
   in State time' taskMap' stepsWorkedOn' stepOrder'

-- I reckon we might as well jsut skip ahead until one of the steps is finished
-- could try naive solution first though

getTasksToStart :: [StepWorkedOn] -> TaskMap -> [Char]
getTasksToStart alreadyInProgress taskMap =
  let candidates = filter (\x -> not (any (\y -> stepId y == x) alreadyInProgress)) . M.keys . M.filter null $ taskMap
   in take (numberOfWorkers - length alreadyInProgress) candidates

findFinishedTasks :: [StepWorkedOn] -> [Char]
findFinishedTasks = map stepId . filter ((== 0) . timeLeft)

removeFinishedTasks :: [StepWorkedOn] -> [StepWorkedOn]
removeFinishedTasks = filter ((/= 0) . timeLeft)

advanceTasks :: Int -> [StepWorkedOn] -> [StepWorkedOn]
advanceTasks i = map (\(StepWorkedOn c y) -> StepWorkedOn c (y - i))

-- should be 5 in real puzzle
numberOfWorkers :: Int
numberOfWorkers = 5

-- should be 61 in real puzzle
quickestTaskTime :: Int
quickestTaskTime = 61

startTasks :: [Char] -> [StepWorkedOn] -> [StepWorkedOn]
startTasks new current = map startTask new ++ current
  where
    taskTimeLookup = M.fromList (zip ['A' .. 'Z'] [quickestTaskTime ..])
    startTask c = StepWorkedOn c (taskTimeLookup M.! c)

findTasksToStart :: Int -> TaskMap -> [Char]
findTasksToStart availableWorkers = take availableWorkers . sort . M.keys . M.filter null

handleFinishedTasks :: [Char] -> TaskMap -> TaskMap
handleFinishedTasks xs tm = foldr handleFinishedTask tm xs
  where
    handleFinishedTask :: Char -> TaskMap -> TaskMap
    handleFinishedTask task = M.map (filter (/= task)) . M.delete task

-- inspired by scala function of the same name
groupMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> M.Map k [v]
groupMap keyBy mapBy =
  M.fromList
    . map (\xs -> (keyBy . head $ xs, map mapBy xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy
