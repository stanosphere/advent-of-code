module Day2 where

import Data.List.Split (splitOn)
import Data.Map qualified as M (Map, elems, findWithDefault, fromList, unionWith)

-- could also use a more precise datatype if I wanted I suppose
-- data Hand = Hand {red :: Int, green :: Int, blue :: Int}
type Hand = M.Map String Int

data Game = Game {gameId :: Int, hands :: [Hand]} deriving (Show)

-- 2156
part1 = do
  xs <- getLines "./fixtures/input2.txt"
  print . sum . map gameId . filter gameIsPossible . map parseLine $ xs

-- 66909
part2 = do
  xs <- getLines "./fixtures/input2.txt"
  print . sum . map (gamePower . parseLine) $ xs

gamePower :: Game -> Int
gamePower = product . M.elems . foldl1 (M.unionWith max) . hands

gameIsPossible :: Game -> Bool
gameIsPossible = all handIsPossible . hands

-- only 12 red cubes, 13 green cubes, and 14 blue cubes
handIsPossible :: Hand -> Bool
handIsPossible hand = (getCount "red" <= 12) && (getCount "green" <= 13) && (getCount "blue" <= 14)
  where
    getCount colour = M.findWithDefault 0 colour hand

-- I really should try using a parser library...
parseLine :: String -> Game
parseLine s =
  let [game, hands] = splitOn ": " s
      game' = read . drop 5 $ game
      hands' = map parseHand . splitOn "; " $ hands
   in Game game' hands'

parseHand :: String -> Hand
parseHand = M.fromList . map ((\[a, b] -> (b, read a)) . splitOn " ") . splitOn ", "

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)