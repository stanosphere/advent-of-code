module Day7 where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import Data.Word (Word16)
import Text.Parsec as P (choice, digit, letter, many1, newline, sepBy, string, try)
import Text.ParserCombinators.Parsec (Parser, parse)

type WireMap = M.Map String Word16

type GateMap = M.Map String Instruction

data Instruction
  = INPUT Word16 String
  | -- AND can take literals too
    AND String String String
  | OR String String String
  | LSHIFT String Word16 String
  | RSHIFT String Word16 String
  | NOT String String
  deriving (Show)

part1 = do
  input <- getInput
  let res = mkGateMap input
  let res' = resolve (map f input) res M.empty
  return res'

mkGateMap :: [Instruction] -> GateMap
mkGateMap = M.fromList . map (\x -> (f x, x))

f :: Instruction -> String
f (INPUT _ x) = x
f (AND _ _ x) = x
f (OR _ _ x) = x
f (LSHIFT _ _ x) = x
f (RSHIFT _ _ x) = x
f (NOT _ x) = x

resolve :: [String] -> GateMap -> WireMap -> WireMap
resolve wireNames gateMap wireMap =
  foldl' (\acc wireName -> resolve' wireName gateMap acc) wireMap wireNames

resolve' :: String -> GateMap -> WireMap -> WireMap
resolve' wireName gateMap wireMap =
  if M.member wireName wireMap
    then wireMap
    else case gateMap M.! wireName of
      (INPUT x _) -> M.insert wireName x wireMap
      (AND in1 in2 _) -> goBin in1 in2 (.&.)
      (OR in1 in2 _) -> goBin in1 in2 (.|.)
      (LSHIFT in1 x _) -> goSing in1 (shiftL' x)
      (RSHIFT in1 x _) -> goSing in1 (shiftR' x)
      (NOT in1 _) -> goSing in1 complement
  where
    resolveInput x = resolve' x gateMap wireMap
    resolveInputs x y = resolve' x gateMap . resolve' y gateMap $ wireMap

    insertNewSing r x op = M.insert wireName (op (r M.! x)) r
    goSing x = insertNewSing (resolveInput x) x

    insertNewBin r x y op = M.insert wireName ((r M.! x) `op` (r M.! y)) r
    goBin x y = insertNewBin (resolveInputs x y) x y

shiftL' :: Word16 -> Word16 -> Word16
shiftL' x y = shiftL y (fromIntegral x)

shiftR' :: Word16 -> Word16 -> Word16
shiftR' x y = shiftR y (fromIntegral x)

getInput :: IO [Instruction]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input7.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy` newline

instructionParser :: Parser Instruction
instructionParser = P.choice . map try $ [wireInputParser, andParser, orParser, lshiftParser, rshiftParser, notParser]
  where
    -- 19138 -> b
    wireInputParser :: Parser Instruction
    wireInputParser =
      INPUT
        <$> (intParser <* string " -> ")
        <*> wireName

    -- hq AND hs -> ht
    andParser :: Parser Instruction
    andParser =
      AND
        <$> (wireName <* string " AND ")
        <*> (wireName <* string " -> ")
        <*> wireName

    -- kg OR kf -> kh
    orParser :: Parser Instruction
    orParser =
      OR
        <$> (wireName <* string " OR ")
        <*> (wireName <* string " -> ")
        <*> wireName

    lshiftParser :: Parser Instruction
    lshiftParser =
      LSHIFT
        <$> (wireName <* string " LSHIFT ")
        <*> (intParser <* string " -> ")
        <*> wireName

    rshiftParser :: Parser Instruction
    rshiftParser =
      RSHIFT
        <$> (wireName <* string " RSHIFT ")
        <*> (intParser <* string " -> ")
        <*> wireName

    notParser :: Parser Instruction
    notParser =
      NOT
        <$> (string "NOT " *> wireName)
        <*> (string " -> " *> wireName)

    intParser :: Parser Word16
    intParser = read <$> many1 digit

    wireName :: Parser String
    wireName = many1 letter
