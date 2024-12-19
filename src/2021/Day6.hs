module Day6 where

import Data.Char (digitToInt)
import qualified Data.Map as M

type DaysBeforeBreeding = Int

type FishCount = Integer

type FishSchool = M.Map DaysBeforeBreeding FishCount

-- naive implementation
part1 :: IO ()
part1 = do
  input <- getInput "./fixtures/input6.txt"
  print . length . applyN 80 step $ input

part1' :: IO ()
part1' = do
  input <- getInput "./fixtures/input6.txt"
  print . M.foldr (+) 0 . applyN 80 step' . getFrequency $ input

part2 :: IO ()
part2 = do
  input <- getInput "./fixtures/input6.txt"
  print . M.foldr (+) 0 . applyN 256 step' . getFrequency $ input

step :: [Int] -> [Int]
step fishes = fishes >>= (\f -> if f == 0 then [6, 8] else [f - 1])

step' :: FishSchool -> FishSchool
step' fishes = M.foldlWithKey birthFish fishes fishes

birthFish :: FishSchool -> DaysBeforeBreeding -> FishCount -> FishSchool
birthFish oldMap days fishCount =
  birthNewFish . ageFish $ oldMap
  where
    birthNewFish =
      if days == 0
        then M.insertWith (+) 8 fishCount . M.insertWith (+) 6 fishCount
        else M.insertWith (+) (days - 1) fishCount
    ageFish = M.insertWith (flip (-)) days fishCount

-- feels rather like `iterate`
applyN :: Int -> (a -> a) -> (a -> a)
applyN 0 _ = id
applyN n f = f . applyN (n - 1) f

-- logic
-- Each iteration:
--   a 0 becomes a 6 and adds a new 8 to the end of the list,
--   while each other number decreases by 1 if it was present at the start of the day

-- ugly parsing stuff below here
getInput :: FilePath -> IO [Int]
getInput filePath = fmap (map digitToInt . filter (/= ',') . head . lines) (readFile filePath)

inputToSchool :: [Int] -> FishSchool
inputToSchool = getFrequency

getFrequency :: (Ord a) => [a] -> M.Map a Integer
getFrequency = foldr (\x -> M.insertWith (+) x 1) M.empty

toyInput :: [Int]
toyInput = [3, 4, 3, 1, 2]
