module Day11 where

import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec as P (alphaNum, char, many1, newline, sepBy, string)
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Grouping (groupMap)

-- this is actually not a million miles away from the splitter problem
-- the one from day 7
-- we need to count paths: I guess this is just a bit more general

-- TODO should probably be Map String (Set String) really
type Edges = M.Map String [String]

type ResultMap = M.Map String Int

data State = State
  { _unResolved :: S.Set String,
    _resolved :: ResultMap
  }
  deriving (Show)

-- for part 2 I observe that what we are dealing with is a DAG
-- thus the nodes we go via cannot form a loop
-- so for all paths we care about we must go through one first, and then the other
-- so they have a fixed order
-- so if I can work out this fixed order then I can just run my `solve` function 3 times
-- being careful to initialize it appropriately each time
-- via experiment i think i need to go svr -> fft -> dac -> out

part2 :: IO (Maybe Int)
part2 = do
  edges <- getInput
  let reversedEdges = getReversedEdges edges

  return (solvePart2 (edges, reversedEdges))

-- need to go svr -> fft -> dac -> out
solvePart2 :: (Edges, Edges) -> Maybe Int
solvePart2 (edges, reverseEdges) =
  Just 1
    >>= solve' "svr" "fft"
    >>= solve' "fft" "dac"
    >>= solve' "dac" "out"
  where
    mkState node paths = State (S.fromList (edges M.! node)) (M.singleton node paths)
    solve' start end paths = solve (edges, reverseEdges) (mkState start paths) end

part1 :: IO (Maybe Int)
part1 = do
  edges <- getInput
  let reversedEdges = getReversedEdges edges
  let initialState = State (S.fromList (edges M.! "you")) (M.singleton "you" 1)
  let res = solve (edges, reversedEdges) initialState "out"

  return res

solve :: (Edges, Edges) -> State -> String -> Maybe Int
solve dag state end =
  fmap ((M.! end) . _resolved)
    . find (null . _unResolved)
    . iterate (step dag)
    $ state

getReversedEdges :: Edges -> Edges
getReversedEdges = groupMap fst snd . concatMap (\(n, cs) -> map (\c -> (c, n)) cs) . M.toList

step :: (Edges, Edges) -> State -> State
step (edges, reversedEdges) (State unResolved resolved) = State unResolved' resolved'
  where
    -- I was slightly concerned we might need to check that each parent is fully resolved before proceeding
    -- and in fact my initial version had such a check
    -- but it would seem this check is not needed
    resolved' = foldl' updateResolved resolved unResolved
    unResolved' = S.unions . S.map (S.fromList . getEdges) $ unResolved

    updateResolved :: ResultMap -> String -> ResultMap
    updateResolved acc n = M.insert n (sum . map pathCount . getReverseEdges $ n) acc
      where
        pathCount :: String -> Int
        pathCount node = M.findWithDefault 0 node resolved

    getEdges :: String -> [String]
    getEdges node = M.findWithDefault [] node edges

    getReverseEdges :: String -> [String]
    getReverseEdges node = M.findWithDefault [] node reversedEdges

getInput :: IO (M.Map String [String])
getInput =
  M.fromList
    . unsafeParse (lineParser `P.sepBy` P.newline)
    <$> readFile "./fixtures/input11.txt"
  where
    lineParser :: Parser (String, [String])
    lineParser =
      (,)
        <$> P.many1 P.alphaNum
        <* P.string ": "
        <*> P.many1 P.alphaNum `P.sepBy` P.char ' '

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res