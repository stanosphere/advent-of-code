module Day7 where

import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import Data.Word (Word16)
import Text.Parsec as P (choice, digit, letter, many1, newline, sepBy, string, try, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

type WireMap = M.Map String Word16

type GateMap = M.Map String Instruction

data Input = Literal Word16 | WireName String deriving (Show)

data Instruction
  = INPUT Input String
  | -- AND can take literals too
    AND Input Input String
  | OR Input Input String
  | LSHIFT Input Word16 String
  | RSHIFT Input Word16 String
  | NOT Input String
  deriving (Show)

part1 :: IO Word16
part1 = do
  input <- getInput
  let res = mkGateMap input
  let res' = resolve (map f input) res M.empty
  return (res' M.! "a")

part2 :: IO Word16
part2 = do
  input <- getInput
  let res = mkGateMap input
  let res' = resolve (map f input) res M.empty
  let valueToSetBAs = res' M.! "a"
  let modifiedInput = map (\i -> if f i == "b" then INPUT (Literal valueToSetBAs) "b" else i) input
  let res'' = mkGateMap modifiedInput
  let res''' = resolve (map f modifiedInput) res'' M.empty
  return (res''' M.! "a")

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
      (INPUT in1 _) -> goNoArg in1
      (AND in1 in2 _) -> goBin in1 in2 (.&.)
      (OR in1 in2 _) -> goBin in1 in2 (.|.)
      (LSHIFT in1 x _) -> goSing in1 (shiftL' x)
      (RSHIFT in1 x _) -> goSing in1 (shiftR' x)
      (NOT in1 _) -> goSing in1 complement
  where
    resolveInput x = resolve' x gateMap wireMap
    resolveInputs x y = resolve' x gateMap . resolve' y gateMap $ wireMap

    insertNewSing r x op = M.insert wireName (op (r M.! x)) r

    goNoArg :: Input -> WireMap
    goNoArg (WireName x) =
      let res' = resolveInput x
          x' = res' M.! x
       in M.insert wireName x' wireMap
    goNoArg (Literal x) = M.insert wireName x wireMap

    goSing :: Input -> (Word16 -> Word16) -> WireMap
    goSing (WireName x) op = insertNewSing (resolveInput x) x op
    goSing (Literal x) op = M.insert wireName (op x) wireMap

    insertNewBin r x y op = M.insert wireName ((r M.! x) `op` (r M.! y)) r

    goBin :: Input -> Input -> (Word16 -> Word16 -> Word16) -> WireMap
    goBin (WireName x) (WireName y) op = insertNewBin (resolveInputs x y) x y op
    goBin (Literal x) (Literal y) op = M.insert wireName (op x y) wireMap
    goBin (WireName x) (Literal y) op =
      let res = resolveInput x
          x' = res M.! x
       in M.insert wireName (op x' y) wireMap
    goBin (Literal x) (WireName y) op =
      let res = resolveInput y
          y' = res M.! y
       in M.insert wireName (op x y') wireMap

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
        <$> (inputWire <* string " -> ")
        <*> many1 letter

    -- hq AND hs -> ht
    andParser :: Parser Instruction
    andParser =
      AND
        <$> (inputWire <* string " AND ")
        <*> (inputWire <* string " -> ")
        <*> many1 letter

    -- kg OR kf -> kh
    orParser :: Parser Instruction
    orParser =
      OR
        <$> (inputWire <* string " OR ")
        <*> (inputWire <* string " -> ")
        <*> many1 letter

    lshiftParser :: Parser Instruction
    lshiftParser =
      LSHIFT
        <$> (inputWire <* string " LSHIFT ")
        <*> (intParser <* string " -> ")
        <*> many1 letter

    rshiftParser :: Parser Instruction
    rshiftParser =
      RSHIFT
        <$> (inputWire <* string " RSHIFT ")
        <*> (intParser <* string " -> ")
        <*> many1 letter

    notParser :: Parser Instruction
    notParser =
      NOT
        <$> (string "NOT " *> inputWire)
        <*> (string " -> " *> many1 letter)

    intParser :: Parser Word16
    intParser = read <$> many1 digit

    inputWire :: Parser Input
    inputWire = try p1 <|> try p2
      where
        p1 = WireName <$> many1 letter
        p2 = Literal <$> intParser
