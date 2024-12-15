module Day3 where

import Data.List.Split (splitOn)
import Data.Map
  ( Map,
    empty,
    fromList,
    keysSet,
    union,
    (!),
  )
import qualified Data.Set as S
  ( Set,
    intersection,
    map,
  )

data Direction = U | D | L | R

data Command = Com {direction :: Direction, steps :: Int}

data Point = P {x :: Int, y :: Int} deriving (Ord, Eq)

type Wire = [Command]

type Wire' = Map Point Int

part1 :: IO ()
part1 = do
  (commonKeys, _, _) <- solve
  print . S.map getManhattanDistance $ commonKeys

part2 :: IO ()
part2 = do
  (commonKeys, wire0, wire1) <- solve
  print . S.map (\k -> (wire0 ! k) + (wire1 ! k)) $ commonKeys

solve :: IO (S.Set Point, Wire', Wire')
solve = do
  file <- readFile $ "./fixtures/input3.txt"
  let [wire0, wire1] = map (commandsToPoints . parseWire) . lines $ file
  let commonKeys = S.intersection (keysSet wire0) (keysSet wire1)
  return (commonKeys, wire0, wire1)

commandsToPoints :: Wire -> Wire'
commandsToPoints x = let (a, _, _) = commandsToPoints' x in a
  where
    commandsToPoints' = foldl processCommand (empty, P 0 0, 0)
    processCommand :: (Wire', Point, Int) -> Command -> (Wire', Point, Int)
    processCommand (oldSet, startPoint, distance) cmd =
      let (newSet, endPoint, newDistance) =
            commandToPoints startPoint distance cmd
       in (oldSet `union` newSet, endPoint, newDistance)

commandToPoints :: Point -> Int -> Command -> (Wire', Point, Int)
commandToPoints point dist command =
  let newPoints = commandToPoints' point dist command
   in (fromList newPoints, fst . last $ newPoints, snd . last $ newPoints)
  where
    commandToPoints' p dist (Com dir steps) =
      map (\i -> (mkPoint p i dir, dist + i)) [0 .. steps]
    mkPoint (P x y) i U = P x (y + i)
    mkPoint (P x y) i D = P x (y - i)
    mkPoint (P x y) i L = P (x - i) y
    mkPoint (P x y) i R = P (x + i) y

parseCommand :: String -> Command
parseCommand ('U' : amount) = Com U (read amount)
parseCommand ('D' : amount) = Com D (read amount)
parseCommand ('L' : amount) = Com L (read amount)
parseCommand ('R' : amount) = Com R (read amount)

parseWire :: String -> Wire
parseWire = map parseCommand . splitOn ","

getManhattanDistance :: Point -> Int
getManhattanDistance (P x y) = (abs x) + (abs y)
