module Day11Alt where

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec as P (alphaNum, char, many1, newline, sepBy, string)
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Grouping (groupMap)

-- this is actually not a million miles away from the splitter problem
-- the one from day 7
-- we need to count paths: I guess this is just a bit more general

type Edges = M.Map String [String]

type ResultMap = M.Map String Int

data State = State
  { _unResolved :: S.Set String,
    _resolved :: ResultMap
  }
  deriving (Show)

part1 :: IO Int
part1 = (\input -> solve input "you" "out") <$> getInput

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