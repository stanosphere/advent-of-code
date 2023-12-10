module Day10 where

import Data.Foldable
import Data.Map qualified as M (Map, findWithDefault, fromList, lookup, notMember, toList, (!))
import Data.Maybe (mapMaybe)

type SymbolMap = M.Map Coords Char

type Curve = M.Map Coords Char

type Coords = (Int, Int)

data Direction = N | E | S | W deriving (Show)

data State = State {direction :: Direction, position :: Coords} deriving (Show)

-- 6842
-- 0.09 secs (including drawing and going through the curve both ways)
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

-- so for part 2 I reckon we ca use the Jordan Curve Theorem
-- https://en.wikipedia.org/wiki/Jordan_curve_theorem
-- maybe I should visualise this too
-- but getting the answer using a weird maths thing is good enough for me
-- 393
-- (0.25 secs,)
part2 :: IO Int
part2 = do
  curve <- getCurve
  let pointsNotOnCurve = [(x, y) | x <- [0 .. 139], y <- [0 .. 139], M.notMember (x, y) curve]
  let x = length . filter (pointIsInsideCurve curve) $ pointsNotOnCurve
  return x

-- (24,76)
getStart :: SymbolMap -> Coords
getStart = fst . get . find ((== 'S') . snd) . M.toList

evolveState :: SymbolMap -> State -> State
evolveState sm (State dir pos) = nextState dir (sm M.! pos) pos
  where
    -- there are common patterns that one could extract out but I reckon case by case is fine!
    -- for example if you end up facing west that's always associated with an x change of `-1` and similar for other directions
    -- like you could have helpers called `moveSouth` etc if you wanted
    nextState :: Direction -> Char -> Coords -> State
    nextState S '└' = moveE
    nextState W '└' = moveN
    --
    nextState S '┘' = moveW
    nextState E '┘' = moveN
    --
    nextState N '┐' = moveW
    nextState E '┐' = moveS
    --
    nextState N '┌' = moveE
    nextState W '┌' = moveS
    --
    nextState E '─' = moveE
    nextState W '─' = moveW
    --
    nextState N '│' = moveN
    nextState S '│' = moveS
    --
    nextState _ _ = undefined

    moveE (x, y) = State E (x + 1, y)
    moveW (x, y) = State W (x - 1, y)
    moveN (x, y) = State N (x, y - 1)
    moveS (x, y) = State S (x, y + 1)

-- this is the bit that uses Jordan curve theorem
-- could definitely do it more efficiently by choosing the shortest direction to the edge
-- or actually probably by directly using the input lists and such
-- also it's not obvious why the list I use is `['┐', '┌', '│']`!
-- answer is I basically can't handle `'─'` tiles so I pretend that each square only exists as its lower half
-- I could just as well have chosen the upper half or indeed done a similar thing in the y direction
-- which ties in with one of my efficiency concerns a little
-- I guess another you could do is like maintain a map of resolved coordinates, that way you'd only need to scan to the nearest resolved coordinate
pointIsInsideCurve :: Curve -> Coords -> Bool
pointIsInsideCurve cc (x0, y) =
  odd
    . length
    . filter (\char -> char `elem` ['┐', '┌', '│'])
    . mapMaybe (\x -> M.lookup (x, y) cc)
    $ [x0 + 1 .. 139]

getCurve :: IO SymbolMap
getCurve = do
  input <- getLines "./fixtures/input10.txt"
  let symbols = getSymbolCoords input
  let startingCoords = (24, 76)
  let statingPoint = (startingCoords, '┐')
  let otherPoints =
        map (\(State _ pos) -> (pos, symbols M.! pos))
          . takeWhile ((/= startingCoords) . position)
          . iterate (evolveState symbols)
          $ State W (23, 76)
  return . M.fromList $ (statingPoint : otherPoints)

-- parsing
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

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
    prettifySymbol _ = undefined

-- utils
get :: Maybe a -> a
get Nothing = undefined
get (Just x) = x

-- pretty printing
prettyPrintSymbolMap :: Int -> SymbolMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[mp M.! (x, y) | x <- counter] | y <- counter]

prettyPrintCurve :: Int -> SymbolMap -> IO ()
prettyPrintCurve size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[M.findWithDefault ' ' (x, y) mp | x <- counter] | y <- counter]