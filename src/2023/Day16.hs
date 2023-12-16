module Day16 where

import Data.Foldable (find, traverse_)
import Data.Map qualified as M (Map, fromList, lookup, (!))
import Data.Set qualified as S (Set, difference, empty, filter, fromList, map, member, singleton, size, union, unions)

type SymbolMap = M.Map Coords Char

-- so I think this is just a more complicated version of day 10
-- for part 1 at least I can probs use a similar approach
-- difference is we have many lasers
-- but they're all independent so it's all chill

type Coords = (Int, Int)

data Direction = N | E | S | W deriving (Show, Eq, Ord)

data LaserState = LS {direction :: Direction, position :: Coords} deriving (Show, Eq, Ord)

type SeenStates = S.Set LaserState

type LaserFrontiers = S.Set LaserState

data FullState = FullState {seen :: SeenStates, laserFrontier :: LaserFrontiers} deriving (Show)

showFullState :: FullState -> IO ()
showFullState (FullState seen laserFrontier) = print "" *> (print . S.size $ seen) *> print laserFrontier

removeOutOfBounds :: (Int, Int) -> S.Set (Int, Int) -> S.Set (Int, Int)
removeOutOfBounds (maxX, maxY) = S.filter (\(x, y) -> x >= 0 && y >= 0 && x < maxX && y < maxY)

-- 7979
-- 0.17 secs
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input16.txt"
  return (runStartingFrom 110 (getSymbolCoords inp) (LS E (0, 0)))

runStartingFrom :: Int -> SymbolMap -> LaserState -> Int
runStartingFrom maxSize symbolMap startingState =
  S.size
    . removeOutOfBounds (maxSize, maxSize)
    . S.map position
    . seen
    . get
    . find ((== S.empty) . laserFrontier)
    . iterate (step symbolMap)
    $ FullState (S.singleton startingState) (S.singleton startingState)

-- so I think we'll want to run the evolveStates function and then get rid of anything we've seen before from the frontier
-- and then we can just run this function until we have no laser frontiers left I hope!
-- but first I'll just iterate my evolveLaserFrontiers function to sense check it...
step :: SymbolMap -> FullState -> FullState
step sm (FullState seen laserFrontier) =
  let evolved = evolveLaserFrontiers sm laserFrontier
      evolved' = S.difference evolved seen
      seen' = S.union evolved' seen
   in FullState seen' evolved'

evolveLaserFrontiers :: SymbolMap -> LaserFrontiers -> LaserFrontiers
evolveLaserFrontiers sm = flatMap (evolveLaserFrontier sm)

evolveLaserFrontier :: SymbolMap -> LaserState -> LaserFrontiers
evolveLaserFrontier sm (LS dir pos) =
  case M.lookup pos sm of
    Just symbol -> S.fromList (nextState dir symbol pos)
    Nothing -> S.empty
  where
    nextState :: Direction -> Char -> Coords -> [LaserState]
    nextState N '/' cs = [moveE cs]
    nextState N '\\' cs = [moveW cs]
    nextState N '-' cs = [moveE cs, moveW cs]
    nextState N '|' cs = [moveN cs]
    nextState N '.' cs = [moveN cs]
    --
    nextState E '/' cs = [moveN cs]
    nextState E '\\' cs = [moveS cs]
    nextState E '-' cs = [moveE cs]
    nextState E '|' cs = [moveN cs, moveS cs]
    nextState E '.' cs = [moveE cs]
    --
    nextState S '/' cs = [moveW cs]
    nextState S '\\' cs = [moveE cs]
    nextState S '-' cs = [moveE cs, moveW cs]
    nextState S '|' cs = [moveS cs]
    nextState S '.' cs = [moveS cs]
    --
    nextState W '/' cs = [moveS cs]
    nextState W '\\' cs = [moveN cs]
    nextState W '-' cs = [moveW cs]
    nextState W '|' cs = [moveN cs, moveS cs]
    nextState W '.' cs = [moveW cs]
    --
    nextState _ _ _ = undefined

    moveE (x, y) = LS E (x + 1, y)
    moveW (x, y) = LS W (x - 1, y)
    moveN (x, y) = LS N (x, y - 1)
    moveS (x, y) = LS S (x, y + 1)

getSymbolCoords :: [String] -> SymbolMap
getSymbolCoords inp = M.fromList [((x, y), c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

prettyPrintSymbolMap :: Int -> SymbolMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[mp M.! (x, y) | x <- counter] | y <- counter]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

prettyPrintLasers :: Int -> S.Set (Int, Int) -> IO ()
prettyPrintLasers size set =
  let counter = [0 .. size - 1]
   in traverse_ putStrLn [[if S.member (x, y) set then '#' else '.' | x <- counter] | y <- counter]

flatMap :: Ord a => (a -> S.Set a) -> S.Set a -> S.Set a
flatMap f = S.unions . S.map f

-- naughty naughty I know
get :: Maybe a -> a
get Nothing = undefined
get (Just x) = x