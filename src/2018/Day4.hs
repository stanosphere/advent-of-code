{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Day4 where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.List.Extra (maximumOn)
import Data.List.Split (chunksOf, splitOn)
import Data.Map qualified as M
  ( Map,
    empty,
    filter,
    fromList,
    map,
    null,
    toList,
    unionWith,
  )

-- use whatever Haskell's equivalent of value calss is for guard id?
data EventType = FallsAsleep | WakesUp | BeginsShift String deriving (Show)

data Event = Event
  { eventType :: EventType,
    time :: DateTime
  }
  deriving (Show)

-- Haskell's built in ones seemed a little cryptic to me!
-- not including year because they're all the same year
data DateTime = DateTime
  { month :: Int,
    day :: Int,
    hour :: Int,
    minute :: Int
  }
  deriving (Show, Ord, Eq)

-- I think the code would be less of a disgrace if i used a more precise type for shigt here
type Shift = [Event]

data ShiftSummary = SS
  { guardId :: String,
    timeAsleep :: Int
  }
  deriving (Show)

part1 :: IO ()
part1 = do
  rawInput <- getLines "./fixtures/input4.txt"
  let shifts = toShifts . sortOn time . map parseEvent $ rawInput
  let guardWithMostminutesAsleep = fst . getGuardWithMostTimeAsleep . map getShiftSummary $ shifts
  let res = maximumOn snd . M.toList . getMinuteMostAsleep . getShiftsForGuard guardWithMostminutesAsleep $ shifts
  print guardWithMostminutesAsleep
  print res

part2 :: IO ()
part2 = do
  rawInput <- getLines "./fixtures/input4.txt"
  print
    . ( maximumOn (snd . snd)
          . M.toList
          . M.map (maximumOn snd . M.toList)
          . M.filter (not . M.null)
          . groupMapReduce guardIdForShift shiftToMinutesAsleep (M.unionWith (+))
      )
    . toShifts
    . sortOn time
    . map parseEvent
    $ rawInput

guardIdForShift :: Shift -> String
guardIdForShift ((Event (BeginsShift guardId) _) : _) = guardId
guardIdForShift _ = undefined

getShiftsForGuard :: String -> [Shift] -> [Shift]
getShiftsForGuard guadId = filter (\s -> guadId == guardIdForShift s)

getMinuteMostAsleep :: [Shift] -> M.Map Int Int
getMinuteMostAsleep = foldl (M.unionWith (+)) M.empty . map shiftToMinutesAsleep

shiftToMinutesAsleep :: Shift -> M.Map Int Int
shiftToMinutesAsleep (_ : xs) =
  M.fromList
    . map (\x -> (x, 1))
    . concatMap (\[sleep, wake] -> [(minute . time $ sleep) .. ((minute . time $ wake) - 1)])
    . chunksOf 2
    $ xs
shiftToMinutesAsleep _ = undefined

getGuardWithMostTimeAsleep :: [ShiftSummary] -> (String, Int)
getGuardWithMostTimeAsleep = maximumOn snd . M.toList . timeSleptByEachGuard

-- timeSleptByEachGuard :: [ShiftSummary] -> M.Map
timeSleptByEachGuard :: [ShiftSummary] -> M.Map String Int
timeSleptByEachGuard = groupMapReduce guardId timeAsleep (+)

-- very assumption heavy function but whatever
getShiftSummary :: Shift -> ShiftSummary
getShiftSummary ((Event (BeginsShift guardId) _) : xs) =
  SS
    guardId
    ( sum
        . map (\[sleep, wake] -> (minute . time $ wake) - (minute . time $ sleep))
        . chunksOf 2
        $ xs
    )
getShiftSummary _ = undefined

-- there must be a way to do this with foldr, I just can't see it!
toShifts :: [Event] -> [[Event]]
toShifts = reverse . map reverse . foldl folder []
  where
    folder :: [[Event]] -> Event -> [[Event]]
    folder [] event = [[event]]
    folder t (Event (BeginsShift guard) time) = [Event (BeginsShift guard) time] : t
    folder (h : t) event = (event : h) : t

groupMapReduce :: Ord k => (a -> k) -> (a -> v) -> (v -> v -> v) -> [a] -> M.Map k v
groupMapReduce keyBy mapBy combine =
  M.fromList
    . map (\xs -> (keyBy . head $ xs, foldl1 combine . map mapBy $ xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy

-- parsing
parseEvent :: String -> Event
parseEvent s =
  let [dateTime, eventType] = splitOn "] " s
   in Event (parseEventType eventType) (parseDateTime . drop 1 $ dateTime)

parseDateTime :: String -> DateTime
parseDateTime s =
  let [date, time] = splitOn " " s
      [_, month, day] = splitOn "-" date
      [hour, minute] = splitOn ":" time
   in DateTime (read month) (read day) (read hour) (read minute)

parseEventType :: String -> EventType
parseEventType "falls asleep" = FallsAsleep
parseEventType "wakes up" = WakesUp
parseEventType s =
  let [_, guardId, _, _] = splitOn " " s
   in BeginsShift guardId

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInput :: [String]
toyInput =
  [ "[1518-11-01 00:00] Guard #10 begins shift",
    "[1518-11-01 00:05] falls asleep",
    "[1518-11-01 00:25] wakes up",
    "[1518-11-01 00:30] falls asleep",
    "[1518-11-01 00:55] wakes up",
    "[1518-11-01 23:58] Guard #99 begins shift",
    "[1518-11-02 00:40] falls asleep",
    "[1518-11-02 00:50] wakes up",
    "[1518-11-03 00:05] Guard #10 begins shift",
    "[1518-11-03 00:24] falls asleep",
    "[1518-11-03 00:29] wakes up",
    "[1518-11-04 00:02] Guard #99 begins shift",
    "[1518-11-04 00:36] falls asleep",
    "[1518-11-04 00:46] wakes up",
    "[1518-11-05 00:03] Guard #99 begins shift",
    "[1518-11-05 00:45] falls asleep",
    "[1518-11-05 00:55] wakes up"
  ]