module Day3Part1 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, try)

type Mult = (Int, Int)

part1 :: IO Int
part1 = processInput <$> getInput

processInput :: String -> Int
processInput = sum . map (uncurry (*)) . catMaybes . unsafeParse inputParser

inputParser :: Parser [Maybe Mult]
inputParser = P.many (Just <$> try multParser P.<|> (P.anyToken $> Nothing))

multParser :: Parser Mult
multParser = do
  _ <- P.string "mul("
  x <- P.many1 P.digit
  _ <- P.char ','
  y <- P.many1 P.digit
  _ <- P.char ')'
  return (read x, read y)

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "this arg is the source name, I guess it's just for error messages or something?" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"