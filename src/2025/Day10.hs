module Day10 where

-- some key observations are
-- - the buttons can be pressed in any order and the result will be the same
-- - pressing a button twice is the same as never pressing it
-- - - so this is effectively a mod 2 problem
-- - - we only need to consider pressing each button zero or one times
-- two approaches come to mind for part 1
-- 1. just brute force it by considering all combos of buttons
-- - - if there are n buttons this would mean checking 2 ^ n combos
-- - - (each button is either pressed or it isn't)
-- - https://stackoverflow.com/questions/75036157/more-efficient-powerset-algorithm-haskell might be helpful
-- 2. use some modified linear algebra approach
-- - - I can see how to write this as a vector + matrix equation
-- - - (or system of equations if you prefer)
-- - - and once you set up the problem like this you can just do gaussian elimination or whatever
-- - - the challenge is I don't yet really know how to handle the mod 2 stuff if that makes sense

-- ok yeah part 2 really is linear algebra
-- hopefully the solutions aren't degenerate or anything lol

import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Text.Parsec as P (char, digit, many1, newline, sepBy, sepEndBy, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.BenchMark (runBenchMark)

data LightState = On | Off deriving (Show, Eq)

type LightMap = M.Map Int LightState

type Button = S.Set Int

type Joltage = [Int]

data Line = Line
  { _desiredLightState :: LightMap,
    _buttons :: [Button],
    _joltage :: Joltage
  }
  deriving (Show)

part1 :: IO ()
part1 = do
  input <- getInput
  runBenchMark (sum . mapMaybe solveLine) input

solveLine :: Line -> Maybe Int
solveLine (Line desiredLightState buttons _) = fmap length . find ((== desiredLightState) . applyButtons allLightsOff) $ buttonConfigs
  where
    buttonConfigs = powerSetInOrder buttons
    allLightsOff = M.map (const Off) desiredLightState

applyButtons :: LightMap -> [Button] -> LightMap
applyButtons = foldl' applyButton
  where
    -- for each button that exists flip the switch at that position
    applyButton :: LightMap -> Button -> LightMap
    applyButton = foldl' (flip (M.adjust flipLight))
      where
        flipLight :: (LightState -> LightState)
        flipLight On = Off
        flipLight Off = On

-- this only returns the proper subsets actually
powerSetInOrder :: [a] -> [[a]]
powerSetInOrder xs = concatMap (`subsequencesOfSize` xs) [1 .. len - 1]
  where
    len = length xs

-- 2 -> [1,2,3,4] -> [[3,4],[2,4],[2,3],[1,4],[1,3],[1,2]]
subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l then [] else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (y : ys) =
      let next = subsequencesBySize ys
       in zipWith
            (++)
            ([] : next)
            (map (map (y :)) next ++ [[]])

getInput :: IO [Line]
getInput = unsafeParse (lineParser `P.sepBy` P.newline) <$> readFile "./fixtures/input10.txt"
  where
    lineParser = Line <$> lightMapParser <*> buttonsParser <*> joltageParser

    lightMapParser :: Parser LightMap
    lightMapParser = M.fromList . zip [0 ..] <$> (P.char '[' *> P.many1 lightStateParser <* P.char ']' <* P.char ' ')
      where
        lightStateParser :: Parser LightState
        lightStateParser = (Off <$ P.char '.') P.<|> (On <$ P.char '#')

    buttonsParser :: Parser [Button]
    buttonsParser = buttonParser `P.sepEndBy` P.char ' '
      where
        buttonParser :: Parser Button
        buttonParser = S.fromList <$> (P.char '(' *> (intParser `P.sepBy` P.char ',') <* P.char ')')

    joltageParser :: Parser Joltage
    joltageParser = P.char '{' *> (intParser `P.sepBy` P.char ',') <* P.char '}'

    intParser :: Parser Int
    intParser = read <$> many1 digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res