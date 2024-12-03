module Day3Part2 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (try)

type Mult = (Int, Int)

data ShouldParse = Do | Don't

-- my first ever stateful parser!
-- state is just whether or not we should parse the mults
type StatefulParser a = P.Parsec String ShouldParse a

part2 :: IO Int
part2 = processInput <$> getInput

-- for part1 I think could use the same code and just not parse the "do"s and "don't"s

processInput :: String -> Int
processInput = sum . map (uncurry (*)) . catMaybes . unsafeParse Do inputParser

inputParser :: StatefulParser [Maybe Mult]
inputParser =
  P.many
    ( try multParser
        P.<|> try doParser
        P.<|> try dontParser
        P.<|> (P.anyToken $> Nothing)
    )

multParser :: StatefulParser (Maybe Mult)
multParser = do
  mult <- multParser'
  shouldParse <- P.getState
  return
    ( case shouldParse of
        Do -> Just mult
        Don't -> Nothing
    )
  where
    -- same as the one from part 1
    multParser' :: StatefulParser Mult
    multParser' = do
      _ <- P.string "mul("
      x <- P.many1 P.digit
      _ <- P.char ','
      y <- P.many1 P.digit
      _ <- P.char ')'
      return (read x, read y)

-- always returns Nothing which makes me feel like there's a better way to model this
doParser :: StatefulParser (Maybe Mult)
doParser = P.string "do()" *> P.putState Do $> Nothing

-- always returns Nothing which makes me feel like there's a better way to model this
dontParser :: StatefulParser (Maybe Mult)
dontParser = P.string "don't()" *> P.putState Don't $> Nothing

unsafeParse :: ShouldParse -> StatefulParser a -> String -> a
unsafeParse initState p s = case P.runParser p initState "" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"