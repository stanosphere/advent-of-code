module Day18 where

import Data.Foldable (traverse_)
import Data.Set qualified as S (Set, empty, fromList, member, toList, union)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

-- I reckon Pick's theorem might just sort us out
-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- unless the tunnels intersect I guess?
-- I'll draw the tunnels and see if that's the case...

data Direction = U | D | L | R deriving (Show)

data Instruction = Ins {dir :: Direction, steps :: Int, colour :: String} deriving (Show)

type Coords = (Int, Int)

data State = State {foundPoints :: S.Set Coords, currentPosition :: Coords}

data GridSize = GS
  { minX :: Int,
    minY :: Int,
    maxX :: Int,
    maxY :: Int
  }
  deriving (Show)

part1 = do
  inp <- getLines "./fixtures/input18.txt"
  let instructions = map parseInstruction inp
  let res = processAllInstructions instructions
  plotPoints res

plotPoints :: State -> IO ()
plotPoints (State foundPoints _) =
  let gs = getGridSize . S.toList $ foundPoints
   in prettyPrintCurve gs
  where
    getGridSize :: [Coords] -> GridSize
    getGridSize =
      foldl
        (\(GS minX minY maxX maxY) (x, y) -> GS (min x minX) (min y minY) (max x maxX) (max y maxY))
        (GS maxBound maxBound minBound minBound)
    prettyPrintCurve :: GridSize -> IO ()
    prettyPrintCurve (GS minX minY maxX maxY) =
      let xCounter = [minX .. maxX]
          yCounter = [minY .. maxY]
       in traverse_ putStrLn [[if S.member (x, y) foundPoints then '█' else ' ' | x <- xCounter] | y <- yCounter]

processAllInstructions :: [Instruction] -> State
processAllInstructions = foldl processInstruction (State S.empty (0, 0))

processInstruction :: State -> Instruction -> State
processInstruction (State ps currentCoords) i =
  let nePoints = getNewPoints currentCoords i
      newCoords = getNewCoords currentCoords i
   in State (S.union ps (S.fromList nePoints)) newCoords

getNewCoords :: Coords -> Instruction -> Coords
getNewCoords (x, y) (Ins U steps _) = (x, y - steps)
getNewCoords (x, y) (Ins D steps _) = (x, y + steps)
getNewCoords (x, y) (Ins L steps _) = (x - steps, y)
getNewCoords (x, y) (Ins R steps _) = (x + steps, y)

getNewPoints :: Coords -> Instruction -> [Coords]
getNewPoints (x, y) (Ins U steps _) = map (\off -> (x, y - off)) [0 .. steps]
getNewPoints (x, y) (Ins D steps _) = map (\off -> (x, y + off)) [0 .. steps]
getNewPoints (x, y) (Ins L steps _) = map (\off -> (x - off, y)) [0 .. steps]
getNewPoints (x, y) (Ins R steps _) = map (\off -> (x + off, y)) [0 .. steps]

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

parseInstruction :: String -> Instruction
parseInstruction = unsafeParse instructionParser

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)