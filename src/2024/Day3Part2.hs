module Day3Part2 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (try)

data Mult = Mult Int Int

data ShouldParse = Do | Don't

-- my first ever stateful parser!
type Parser a = P.Parsec String ShouldParse a

solve :: IO Int
solve = processInput <$> getInput

processInput :: String -> Int
processInput = sum . map (\(Mult x y) -> x * y) . catMaybes . unsafeParse inputParser

inputParser :: Parser [Maybe Mult]
inputParser =
  P.many
    ( try multParser
        P.<|> try doParser
        P.<|> try dontParser
        P.<|> (P.anyToken $> Nothing)
    )

multParser :: Parser (Maybe Mult)
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
    multParser' :: Parser Mult
    multParser' = do
      _ <- P.string "mul("
      x <- P.many1 P.digit
      _ <- P.char ','
      y <- P.many1 P.digit
      _ <- P.char ')'
      return (Mult (read x) (read y))

-- always returns Nothing which makes me feel like there's a better way to model this
doParser :: Parser (Maybe Mult)
doParser = P.string "do()" *> P.putState Do $> Nothing

-- always returns Nothing which makes me feel like there's a better way to model this
dontParser :: Parser (Maybe Mult)
dontParser = P.string "don't()" *> P.putState Don't $> Nothing

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case P.runParser p Do "" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"