module Day15 where

import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map qualified as M (Map, adjust, alter, empty, toList)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- using list because ordering matters
type Boxes = M.Map Int [(String, Int)]

data Operation = Remove | Put Int

data Instruction = Ins {boxLabel :: Int, lensLabel :: String, op :: Operation}

example :: String
example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

-- 271384
-- 0.05 secs
part2 :: IO Int
part2 =
  sum
    . map boxToNumber
    . M.toList
    . foldl process M.empty
    . map parseInstruction
    . splitOn ","
    <$> readFile "./fixtures/input15.txt"

boxToNumber :: (Int, [(String, Int)]) -> Int
boxToNumber (boxNumber, lenses) = (* (boxNumber + 1)) . sum . zipWith (*) [1 :: Int ..] . map snd . reverse $ lenses

process :: Boxes -> Instruction -> Boxes
process boxes (Ins boxLabel lensLabel Remove) = M.adjust (filter ((/= lensLabel) . fst)) boxLabel boxes
process boxes (Ins boxLabel lensLabel (Put focalLength)) = alter' (putFnIfJust (lensLabel, focalLength)) boxLabel boxes
  where
    putFnIfJust :: (String, Int) -> Maybe [(String, Int)] -> [(String, Int)]
    putFnIfJust (s, i) (Just xs) =
      if any ((== s) . fst) xs
        then map (\(s', i') -> if s' == s then (s, i) else (s', i')) xs
        else (s, i) : xs
    putFnIfJust (s, i) Nothing = [(s, i)]
    alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

-- 506869
-- 0.03 secs
part1 :: IO Int
part1 =
  sum
    . map hash
    . splitOn ","
    <$> readFile "./fixtures/input15.txt"

hash :: String -> Int
hash = foldl (\n c -> (`rem` 256) . (* 17) . (+ n) . ord $ c) 0

-- parsing stuff below here
parseInstruction :: String -> Instruction
parseInstruction = unsafeParse instructionParser

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

instructionParser :: Parser Instruction
instructionParser = do
  label <- P.many P.letter
  op <- removeParser P.<|> putParser
  return (Ins (hash label) label op)
  where
    removeParser = Remove <$ P.char '-'
    putParser = P.char '=' *> (Put <$> intParser)

intParser :: Parser Int
intParser = read <$> P.many1 P.digit