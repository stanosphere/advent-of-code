module Day3Part1 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, try)

data Mult = Mult Int Int

solve :: IO Int
solve = processInput <$> getInput

processInput :: String -> Int
processInput = sum . map (\(Mult x y) -> x * y) . catMaybes . unsafeParse inputParser

inputParser :: Parser [Maybe Mult]
inputParser = P.many (Just <$> try multParser P.<|> (P.anyToken $> Nothing))

multParser :: Parser Mult
multParser = do
  _ <- P.string "mul("
  x <- P.many1 P.digit
  _ <- P.char ','
  y <- P.many1 P.digit
  _ <- P.char ')'
  return (Mult (read x) (read y))

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "still don't really know wht this arg is for lol" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"