module Day3Part2 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Parsec as P (Parsec, anyToken, char, digit, getState, many, many1, putState, runParser, string, (<|>))
import Text.ParserCombinators.Parsec (try)

type Mult = (Int, Int)

data ShouldParse = Do | Don't

-- my first ever stateful parser!
-- state is just whether or not we should parse the mults
type StatefulParser a = Parsec String ShouldParse a

part2 :: IO Int
part2 = processInput <$> getInput

-- for part1 I think could use the same code and just not parse the "do"s and "don't"s

processInput :: String -> Int
processInput = sum . map (uncurry (*)) . catMaybes . unsafeParse Do inputParser

inputParser :: StatefulParser [Maybe Mult]
inputParser =
  many
    ( try multParser
        <|> try doParser
        <|> try dontParser
        <|> (anyToken $> Nothing)
    )

multParser :: StatefulParser (Maybe Mult)
multParser = do
  mult <- multParser'
  shouldParse <- getState
  return
    ( case shouldParse of
        Do -> Just mult
        Don't -> Nothing
    )
  where
    -- same as the one from part 1
    multParser' :: StatefulParser Mult
    multParser' = (,) <$> (string "mul(" *> intParser) <* char ',' <*> (intParser <* char ')')

    intParser = read <$> many1 digit

-- always returns Nothing which makes me feel like there's a better way to model this
doParser :: StatefulParser (Maybe Mult)
doParser = string "do()" *> putState Do $> Nothing

-- always returns Nothing which makes me feel like there's a better way to model this
dontParser :: StatefulParser (Maybe Mult)
dontParser = string "don't()" *> putState Don't $> Nothing

unsafeParse :: ShouldParse -> StatefulParser a -> String -> a
unsafeParse initState p s = case runParser p initState "" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"