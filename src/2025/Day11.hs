module Day11 where

import Data.Foldable
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

part1 :: IO (Maybe Int)
part1 = do
  edges <- getInput
  let reversedEdges = getReversedEdges edges
  let res = fmap ((M.! "out") . _resolved) . find (null . _unResolved) . iterate (step (edges, reversedEdges)) $ (State (S.fromList (edges M.! "you")) (M.singleton "you" 1))

  return res

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

-- this is a solution that I found on the internet that I don't understand
-- will put it in its own file...
solve :: Ord a => M.Map a [a] -> a -> a -> Int
solve stuff start end = res M.! start
  where
    res =
      M.map (sum . map (\y -> if y == end then 1 else M.findWithDefault 0 y res)) stuff

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