module Day18 where

import Numeric (readHex)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))
import Utils.Grouping (pairs)

-- I reckon Pick's theorem might just sort us out
-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- unless the tunnels intersect I guess?
-- I'll draw the tunnels and see if that's the case...
-- I think what we actually need instead is the shoelace formula
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- we need both formulas...

data Direction = U | D | L | R deriving (Show)

data Instruction = Ins {dir :: Direction, steps :: Int, colour :: String} deriving (Show)

type Coords = (Int, Int)

part1 = do
  inp <- getLines "./fixtures/input18.txt"
  let instructions = map (unsafeParse instructionParser) inp
  -- need to add 4 to apply pick's theorem
  -- I have drawn thi out on square paper and i can see that indeed 4 more is needed than the step count
  -- ut I don't yet fully understand why
  let exteriorPoints = (sum . map steps $ instructions) + 4
  let interiorPoints = shoelaceFormula . processAllInstructions $ instructions
  let area = pickFormula interiorPoints exteriorPoints
  print area

part2 = do
  inp <- getLines "./fixtures/input18.txt"
  let instructions = map (unsafeParse instructionParser') inp
  -- need to add 4 to apply pick's theorem
  -- I have drawn thi out on square paper and i can see that indeed 4 more is needed than the step count
  -- ut I don't yet fully understand why
  let exteriorPoints = (sum . map steps $ instructions) + 4
  let interiorPoints = shoelaceFormula . processAllInstructions $ instructions
  let area = pickFormula interiorPoints exteriorPoints
  print area

pickFormula :: Int -> Int -> Int
pickFormula interiorPoints exteriorPoints = (interiorPoints + exteriorPoints `div` 2) - 1

-- so really we should add a final area contrib but...
-- either both x1 and x2 will be 0 or both y1 and y2 will be zero since we're dealing with straight lines
shoelaceFormula :: [(Int, Int)] -> Int
shoelaceFormula = (`div` 2) . sum . map toAreaUnit . pairs
  where
    toAreaUnit ((x1, y1), (x2, y2)) = (y1 + y2) * (x1 - x2)

processAllInstructions :: [Instruction] -> [Coords]
processAllInstructions = scanl getNewCoords (0, 0)

getNewCoords :: Coords -> Instruction -> Coords
getNewCoords (x, y) (Ins U steps _) = (x, y - steps)
getNewCoords (x, y) (Ins D steps _) = (x, y + steps)
getNewCoords (x, y) (Ins L steps _) = (x - steps, y)
getNewCoords (x, y) (Ins R steps _) = (x + steps, y)

instructionParser' :: Parser Instruction
instructionParser' = do
  _ <- directionParser
  P.space
  _ <- intParser
  P.space
  colour <- colourParser
  return . colourToInstruction $ colour
  where
    colourParser :: Parser String
    colourParser = P.char '(' *> hexParser <* P.char ')'

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit

    hexParser :: Parser String
    hexParser = P.char '#' *> P.many1 (P.oneOf "abcdef0123456789")

    directionParser :: Parser Direction
    directionParser = (U <$ P.char 'U') <|> (D <$ P.char 'D') <|> (L <$ P.char 'L') <|> (R <$ P.char 'R')

    -- Each hexadecimal code is six hexadecimal digits long.
    -- The first five hexadecimal digits encode the distance in meters as a five-digit hexadecimal number.
    -- The last hexadecimal digit encodes the direction to dig: 0 means R, 1 means D, 2 means L, and 3 means U.
    colourToInstruction :: String -> Instruction
    colourToInstruction h =
      let steps = readHex' . take 5 $ h
          dir = case h !! 5 of
            '0' -> R
            '1' -> D
            '2' -> L
            '3' -> U
            _ -> undefined
       in Ins dir steps h

    readHex' :: String -> Int
    readHex' = fst . head . readHex

-- parsing stuff
instructionParser :: Parser Instruction
instructionParser = do
  dir <- directionParser
  P.space
  steps <- intParser
  P.space
  colour <- colourParser
  return (Ins dir steps colour)
  where
    colourParser :: Parser String
    colourParser = P.char '(' *> hexParser <* P.char ')'

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit

    hexParser :: Parser String
    hexParser = P.char '#' *> P.many1 (P.oneOf "abcdef0123456789")

    directionParser :: Parser Direction
    directionParser = (U <$ P.char 'U') <|> (D <$ P.char 'D') <|> (L <$ P.char 'L') <|> (R <$ P.char 'R')

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)