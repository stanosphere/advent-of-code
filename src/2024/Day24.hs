module Day24 where

import Data.Bifunctor (first)
import Data.Bits (xor, (.&.), (.|.))
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'), traverse_)
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec as P
  ( choice,
    digit,
    letter,
    many1,
    newline,
    parse,
    sepBy,
    string,
    try,
    (<|>),
  )
import Text.ParserCombinators.Parsec (Parser)

data Gate
  = OR String String String
  | AND String String String
  | XOR String String String
  deriving (Show)

data InputWire = InputWire String Int deriving (Show)

type WireMap = M.Map String Int

type GateMap = M.Map String Gate

part1 :: IO ()
part1 = do
  (inputWires, gates) <- getInput
  let initMap = mkInitialMap inputWires
  let gateMap = mkGateMap gates
  let wireNames = unresolvedWireNames inputWires gates
  let finalWireMap = resolve wireNames gateMap initMap

  let blah =
        toDecimal
          . map snd
          . sortOn fst
          . map (first (readInt . tail))
          . M.toList
          . M.filterWithKey (\k _ -> (== 'z') . head $ k)
          $ finalWireMap
  traverse_ print . M.toList $ finalWireMap
  print blah

toDecimal :: [Int] -> Int
toDecimal = foldr (\x y -> x + y * 2) 0

readInt :: String -> Int
readInt = read

mkGateMap :: [Gate] -> GateMap
mkGateMap = M.fromList . map (\x -> (outputWire x, x))
  where
    outputWire (OR _ _ x) = x
    outputWire (AND _ _ x) = x
    outputWire (XOR _ _ x) = x

-- I could do a topological sort I guess but where's the fun in that!

mkInitialMap :: [InputWire] -> WireMap
mkInitialMap = M.fromList . map (\(InputWire name value) -> (name, value))

resolve :: [String] -> GateMap -> WireMap -> WireMap
resolve wireNames gateMap wireMap =
  foldl' (\acc wireName -> resolve' wireName gateMap acc) wireMap wireNames

resolve' :: String -> GateMap -> WireMap -> WireMap
resolve' wireName gateMap wireMap =
  if M.member wireName wireMap
    then wireMap
    else case gateMap M.! wireName of
      (OR in1 in2 _) -> go in1 in2 (.|.)
      (AND in1 in2 _) -> go in1 in2 (.&.)
      (XOR in1 in2 _) -> go in1 in2 xor
  where
    resolveInputs x y = resolve' x gateMap . resolve' y gateMap $ wireMap
    insertNew r x y op = M.insert wireName ((r M.! x) `op` (r M.! y)) r
    go x y = insertNew (resolveInputs x y) x y

unresolvedWireNames :: [InputWire] -> [Gate] -> [String]
unresolvedWireNames wires gates = S.toList (ys S.\\ xs)
  where
    xs = S.fromList . map (\(InputWire name _) -> name) $ wires
    ys = S.fromList . concatMap wireNames $ gates
    wireNames :: Gate -> [String]
    wireNames (OR x y z) = [x, y, z]
    wireNames (XOR x y z) = [x, y, z]
    wireNames (AND x y z) = [x, y, z]

getInput :: IO ([InputWire], [Gate])
getInput = unsafeParse inputParser <$> readFile "./fixtures/input24.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser ([InputWire], [Gate])
inputParser = do
  wires <- many1 (wireParser <* newline)
  _ <- newline
  gates <- gateParser `sepBy` newline
  return (wires, gates)
  where
    wireParser :: Parser InputWire
    wireParser =
      InputWire <$> (wireName <* string ": ") <*> (digitToInt <$> digit)

    gateParser :: Parser Gate
    gateParser = choice . map try $ [orGateParser, xorGateParser, andGateParser]
      where
        orGateParser :: Parser Gate
        orGateParser =
          OR <$> (wireName <* string " OR ") <*> (wireName <* string " -> ") <*> wireName

        xorGateParser :: Parser Gate
        xorGateParser =
          XOR <$> (wireName <* string " XOR ") <*> (wireName <* string " -> ") <*> wireName

        andGateParser :: Parser Gate
        andGateParser =
          AND <$> (wireName <* string " AND ") <*> (wireName <* string " -> ") <*> wireName

    wireName :: Parser String
    wireName = many1 (letter <|> digit)