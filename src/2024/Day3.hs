module Day3 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, try)

data Mult = Mult Int Int

part1 :: IO Int
part1 = sum . map processLine <$> getInput

processLine :: String -> Int
processLine = sum . map (\(Mult x y) -> x * y) . catMaybes . unsafeParse inputParser

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

exampleString :: String
exampleString = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input4.txt"