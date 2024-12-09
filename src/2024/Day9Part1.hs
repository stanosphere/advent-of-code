module Day9Part1 where

import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Maybe (catMaybes, isJust)

part1 :: IO Int
part1 = checksum . reOrder . expand <$> getInput

checksum :: [(Int, Space)] -> Int
checksum = sum . map f
  where
    f (_, Free) = 0
    f (i, File j) = i * j

-- could just use Maybe tbh
data Space = Free | File Int deriving (Show, Eq)

reOrder :: [Space] -> [(Int, Space)]
reOrder xs = sortOn fst (_reversedValueList finalState ++ _newValues finalState)
  where
    finalState = last . catMaybes . takeWhile isJust . iterate (wrap transform) . Just . mkInitialState $ xs

mkInitialState :: [Space] -> TransformState
mkInitialState xs = TS holes (reverse values) []
  where
    zipped = zip [0 ..] xs
    holes = map fst . filter ((== Free) . snd) $ zipped
    values = filter ((/= Free) . snd) zipped

transform :: TransformState -> Maybe TransformState
transform (TS (hIndex : holes) ((vIndex, space) : reversedValueList) newValues) =
  if hIndex < vIndex
    then Just (TS holes reversedValueList ((hIndex, space) : newValues))
    else Nothing
transform _ = Nothing

data TransformState = TS
  { _holes :: [Int],
    _reversedValueList :: [(Int, Space)],
    _newValues :: [(Int, Space)]
  }
  deriving (Show)

expand :: [Char] -> [Space]
expand = concatMap f . zip zipper . map digitToInt
  where
    zipper = iterate (\(isFile, i) -> (not isFile, if not isFile then i + 1 else i)) (True, 0)
    f ((isFile, fileIndex), n) = replicate n (if isFile then File fileIndex else Free)

getInput :: IO String
getInput = readFile "./fixtures/input9.txt"

example :: String
example = "2333133121414131402"

-- surely something like this is in the standard lib...
wrap :: (a -> Maybe a) -> (Maybe a -> Maybe a)
wrap f = g
  where
    g Nothing = Nothing
    g (Just x) = f x