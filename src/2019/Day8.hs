module Day8 where

import Data.Foldable (traverse_)
import Data.List.Extra (minimumOn)
import Data.List.Split (chunksOf)

type Layer = [String]

data Pixel = Black | White | Transparent

type Layer' = [[Pixel]]

part1 :: IO Int
part1 = (\x -> countDigits '1' x * countDigits '2' x) . minimumOn (countDigits '0') <$> getInput
  where
    countDigits digit = length . filter (== digit) . concat

part2 :: IO ()
part2 = display . solve . reverse =<< getInput

display :: Layer' -> IO ()
display = traverse_ (putStrLn . map toDisplayChar)
  where
    toDisplayChar White = '█'
    toDisplayChar _ = ' '

solve :: [Layer] -> Layer'
solve = foldl1 combineLayers . map toLayer'

toLayer' :: Layer -> Layer'
toLayer' = map (map toPixel)
  where
    toPixel :: Char -> Pixel
    toPixel '0' = Black
    toPixel '1' = White
    toPixel '2' = Transparent
    toPixel x = error ("unexpected number: " ++ show x)

combineLayers :: Layer' -> Layer' -> Layer'
combineLayers = zipWith (zipWith combinePixels)
  where
    combinePixels :: Pixel -> Pixel -> Pixel
    combinePixels x Transparent = x
    combinePixels _ x = x

-- The image you received is 25 pixels wide and 6 pixels tall.
parseInput :: String -> [Layer]
parseInput = chunksOf 6 . chunksOf 25

getInput :: IO [Layer]
getInput = parseInput <$> readFile "./fixtures/input8.txt"