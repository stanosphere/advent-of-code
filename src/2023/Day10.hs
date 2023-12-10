module Day10 where

import Data.Foldable
import Data.Map qualified as M (Map, findWithDefault, fromList, toList, (!))

type SymbolMap = M.Map Coords Char

type Coords = (Int, Int)

data Direction = N | E | S | W deriving (Show)

data State = State {direction :: Direction, position :: Coords} deriving (Show)

-- so for part 2 I reckon we ca use the Jordan Curve Theorem
-- https://en.wikipedia.org/wiki/Jordan_curve_theorem

evolveState :: SymbolMap -> State -> State
evolveState sm (State dir pos) = nextState pos dir (sm M.! pos)

-- there are common patterns that one could extract out but I reckon case by case is fine!
-- for example if you end up facing west that's always associated with an x change of `-1` and similar for other directions
-- like you could have helpers called `moveSouth` etc if you wanted
nextState :: Coords -> Direction -> Char -> State
nextState (x, y) S '└' = State E (x + 1, y)
nextState (x, y) W '└' = State N (x, y - 1)
nextState (x, y) E '┘' = State N (x, y - 1)
nextState (x, y) S '┘' = State W (x - 1, y)
nextState (x, y) N '┐' = State W (x - 1, y)
nextState (x, y) E '┐' = State S (x, y + 1)
nextState (x, y) N '┌' = State E (x + 1, y)
nextState (x, y) W '┌' = State S (x, y + 1)
nextState (x, y) E '─' = State E (x + 1, y)
nextState (x, y) W '─' = State W (x - 1, y)
nextState (x, y) N '│' = State N (x, y - 1)
nextState (x, y) S '│' = State S (x, y + 1)
nextState _ _ _ = undefined

getCurve :: IO SymbolMap
getCurve = do
  input <- getLines "./fixtures/input10.txt"
  let symbols = getSymbolCoords input
  let startingCoords = (24, 76)
  let otherPoints = takeWhile ((/= startingCoords) . position) . iterate (evolveState symbols) $ State W (23, 76)
  let curve = M.fromList (((24, 76), '┐') : map (\(State _ pos) -> (pos, symbols M.! pos)) otherPoints)
  return curve

-- 6842
part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input10.txt"
  let symbols = getSymbolCoords input
  let startingCoords = (24, 76)
  let res1 = takeWhile ((/= startingCoords) . position) . iterate (evolveState symbols) $ State W (23, 76)
  let res2 = takeWhile ((/= startingCoords) . position) . iterate (evolveState symbols) $ State S (24, 77)
  prettyPrintSymbolMap 139 symbols
  -- both lengths should be the same
  print . length $ res1
  print . length $ res2
  let answer = if odd (length res1) then length res1 `div` 2 + 1 else length res1 `div` 2
  print answer

-- (24,76)
getStart :: SymbolMap -> Coords
getStart = fst . get . find ((== 'S') . snd) . M.toList

getSymbolCoords :: [String] -> SymbolMap
getSymbolCoords inp = M.fromList [((x, y), prettifySymbol c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]
  where
    prettifySymbol :: Char -> Char
    prettifySymbol 'L' = '└'
    prettifySymbol 'J' = '┘'
    prettifySymbol '7' = '┐'
    prettifySymbol 'F' = '┌'
    prettifySymbol '-' = '─'
    prettifySymbol '|' = '│'
    prettifySymbol '.' = ' '
    prettifySymbol 'S' = '┐'

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

prettyPrintSymbolMap :: Int -> SymbolMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[mp M.! (x, y) | x <- counter] | y <- counter]

prettyPrintCurve :: Int -> SymbolMap -> IO ()
prettyPrintCurve size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[M.findWithDefault ' ' (x, y) mp | x <- counter] | y <- counter]

get :: Maybe a -> a
get Nothing = undefined
get (Just x) = x