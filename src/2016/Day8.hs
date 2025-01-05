module Day8 where

import Data.Foldable (traverse_)
import Data.List (foldl', sortOn)
import qualified Data.Map as M
import Text.Parsec as P (char, choice, digit, many1, newline, parse, sepBy, string, try)
import Text.ParserCombinators.Parsec (Parser)

data Command
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving (Show)

data Pixel = On | Off deriving (Eq, Show)

type Coord = (Int, Int)

type Screen = M.Map Coord Pixel

part1 :: IO Int
part1 = M.size . M.filter (== On) . foldl execute initialScreen <$> getInput

part2 :: IO ()
part2 = (>>= display) $ foldl execute initialScreen <$> getInput

display :: Screen -> IO ()
display screen = putStrLn "" *> traverse_ putStrLn [[toChar (x, y) | x <- [0 .. screenWidth - 1]] | y <- [0 .. screenHeight - 1]]
  where
    toChar c = case screen M.! c of
      On -> '█'
      Off -> ' '

screenWidth :: Int
screenWidth = 50

screenHeight :: Int
screenHeight = 6

initialScreen :: Screen
initialScreen = M.fromList [((x, y), Off) | x <- [0 .. screenWidth - 1], y <- [0 .. screenHeight - 1]]

execute :: Screen -> Command -> Screen
execute s (Rect w h) = foldr (`M.insert` On) s [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
execute s (RotateRow y amount) = insertRow . rotateRow . getRow $ s
  where
    getRow = map snd . sortOn (fst . fst) . filter ((== y) . snd . fst) . M.toList
    rotateRow = take screenWidth . drop (screenWidth - amount) . cycle
    insertRow = foldl' (\s' (x, p) -> M.insert (x, y) p s') s . zip [0 ..]
execute s (RotateCol x amount) = insertCol . rotateCol . getCol $ s
  where
    getCol = map snd . sortOn (snd . fst) . filter ((== x) . fst . fst) . M.toList
    rotateCol = take screenHeight . drop (screenHeight - amount) . cycle
    insertCol = foldl' (\s' (y, p) -> M.insert (x, y) p s') s . zip [0 ..]

getInput :: IO [Command]
getInput = unsafeParse (lineParser `sepBy` newline) <$> readFile "./fixtures/input8.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

lineParser :: Parser Command
lineParser = choice . map try $ [rect, rotateRow, rotateCol]
  where
    rect = Rect <$> (string "rect " *> int) <*> (char 'x' *> int)
    rotateRow = RotateRow <$> (string "rotate row y=" *> int) <*> (string " by " *> int)
    rotateCol = RotateCol <$> (string "rotate column x=" *> int) <*> (string " by " *> int)

    int = read <$> many1 digit