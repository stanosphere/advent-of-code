{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Day4 where

import Data.Foldable
import Data.List (sortOn)
import Data.List.Split (splitOn)

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

part1 :: IO ()
part1 = do
  rawInput <- getLines "./fixtures/input4.txt"
  let sortedEvents = sortOn time . map parseEvent $ rawInput
  traverse_ print sortedEvents

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
