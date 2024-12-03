{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Day4 where

import Data.List (sortOn)
import Data.List.Extra (maximumOn)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
  ( Map,
    empty,
    filter,
    fromList,
    map,
    null,
    toList,
    unionWith,
  )
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))
import Utils.Grouping (groupMapReduce)

-- use whatever Haskell's equivalent of value calss is for guard id?
data EventType = FallsAsleep | WakesUp | BeginsShift Int deriving (Show)

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

-- I think the code would be less of a disgrace if i used a more precise type for shift here
type Shift = [Event]

data ShiftSummary = SS
  { guardId :: Int,
    timeAsleep :: Int
  }
  deriving (Show)

part1 :: IO ()
part1 = do
  rawInput <- getLines "./fixtures/input4.txt"
  let shifts = toShifts . sortOn time . map parseEvent $ rawInput
  let guardWithMostminutesAsleep = fst . getGuardWithMostTimeAsleep . map getShiftSummary $ shifts
  let (minute, _) = maximumOn snd . M.toList . getMinuteMostAsleep . getShiftsForGuard guardWithMostminutesAsleep $ shifts
  print (minute * guardWithMostminutesAsleep)

part2 :: IO ()
part2 = do
  rawInput <- getLines "./fixtures/input4.txt"
  print
    . guardMostFrequentlyAsleepInSameMinute
    . toShifts
    . sortOn time
    . map parseEvent
    $ rawInput

guardMostFrequentlyAsleepInSameMinute :: [Shift] -> Int
guardMostFrequentlyAsleepInSameMinute shifts = guardId * minute
  where
    (guardId, (minute, _)) =
      maximumOn (snd . snd)
        . M.toList
        . M.map (maximumOn snd . M.toList)
        . M.filter (not . M.null)
        . groupMapReduce guardIdForShift shiftToMinutesAsleep (M.unionWith (+))
        $ shifts

guardIdForShift :: Shift -> Int
guardIdForShift ((Event (BeginsShift guardId) _) : _) = guardId
guardIdForShift _ = undefined

getShiftsForGuard :: Int -> [Shift] -> [Shift]
getShiftsForGuard guadId = filter ((== guadId) . guardIdForShift)

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

getGuardWithMostTimeAsleep :: [ShiftSummary] -> (Int, Int)
getGuardWithMostTimeAsleep = maximumOn snd . M.toList . timeSleptByEachGuard

-- timeSleptByEachGuard :: [ShiftSummary] -> M.Map
timeSleptByEachGuard :: [ShiftSummary] -> M.Map Int Int
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

-- parsing
parseEvent :: String -> Event
parseEvent = unsafeParse eventParser

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

-- parsing stuff
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "still don't really know wht this arg is for lol" s of
  Left res -> error . show $ res
  Right res -> res

eventParser :: Parser Event
eventParser = do
  dateTime <- dateTimeParser
  P.spaces
  eventType <- eventTypeParser
  return (Event eventType dateTime)

-- "[1518-11-01 00:00]""
dateTimeParser :: Parser DateTime
dateTimeParser = do
  P.string "[1518-"
  month <- intParser
  P.char '-'
  day <- intParser
  P.space
  hour <- intParser
  P.char ':'
  minute <- intParser
  P.char ']'
  return (DateTime month day hour minute)

eventTypeParser :: Parser EventType
eventTypeParser = fallsAsleepParser <|> wakesUpParse <|> beginsShiftParser
  where
    fallsAsleepParser = FallsAsleep <$ P.string "falls asleep"
    wakesUpParse = WakesUp <$ P.string "wakes up"
    beginsShiftParser = P.string "Guard #" *> (BeginsShift <$> intParser) <* P.string " begins shift"

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

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