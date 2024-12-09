module Day9Part2 where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (find, traverse_)
import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes, isJust)

-- part1 :: IO Int
-- part1 = checksum . reOrder . expand <$> getInput

checksum :: [(Int, Space)] -> Int
checksum = sum . map f
  where
    f (_, Free) = 0
    f (i, File j) = i * j

-- could just use Maybe tbh
data Space = Free | File Int deriving (Show, Eq)

showTransformState :: TransformState -> String
showTransformState (TS ws xs ys zs) = concat . map (\(i, n, sp) -> replicate n (case sp of Free -> '.'; (File x) -> intToDigit x)) . sortOn (\(i, _, _) -> i) $ (ws ++ xs ++ ys ++ zs)

showTransformState' :: TransformState -> String
showTransformState' (TS ws xs ys zs) = show xs

reOrder xs = map showTransformState . catMaybes . takeWhile isJust . iterate (wrap transform) . Just . mkInitialState $ xs

mkInitialState :: [(Int, Int, Space)] -> TransformState
mkInitialState xs = TS holes [] (reverse values) []
  where
    holes = filter (\(_, _, space) -> space == Free) xs
    values = filter (\(_, _, space) -> space /= Free) xs

transform :: TransformState -> Maybe TransformState
transform (TS _ _ [] _) = Nothing
transform (TS fillableHoles unFillableHoles ((fileIndex, fileSize, fileId) : otherFiles) newValues) =
  case find (\(_, holeSize, _) -> holeSize >= fileSize) fillableHoles of
    Nothing ->
      Just
        ( TS
            fillableHoles
            unFillableHoles
            otherFiles
            ((fileIndex, fileSize, fileId) : newValues)
        )
    Just (holeIndex, holeSize, _) ->
      Just
        ( TS
            (filHole (holeIndex, holeSize) fileSize fillableHoles)
            ((fileIndex, fileSize, Free) : unFillableHoles)
            otherFiles
            ((holeIndex, fileSize, fileId) : newValues)
        )

filHole :: (Int, Int) -> Int -> [(Int, Int, Space)] -> [(Int, Int, Space)]
filHole (holeIndex, holeSize) fileSize holeList =
  if holeSize == fileSize
    then before ++ after
    else before ++ [(holeIndex + holeSize - fileSize, holeSize - fileSize, Free)] ++ after
  where
    -- could have used splitOn maybe??
    before = takeWhile (\(x, y, z) -> x < holeIndex) holeList
    after = dropWhile (\(x, y, z) -> x <= holeIndex) holeList

-- look for holes, if you can find one then great! fill her up
-- if not then just put the file in the newValues list, it has basically been left alone

data TransformState = TS
  { _fillableHoles :: [(Int, Int, Space)], -- holes don't really need the space in them I guess
    _unFillableHoles :: [(Int, Int, Space)],
    _reversedFileList :: [(Int, Int, Space)],
    _newValues :: [(Int, Int, Space)]
  }
  deriving (Show)

-- blah = groupBy f . zip [0 ..]
--   where
--     f :: (Int, Space) -> (Int, Space) -> Bool
--     f (_, x) (_, y) = g x y
--     g :: Space -> Space -> Bool
--     g Free Free = True
--     g (File _) (File _) = True
--     g Free (File _) = False
--     g (File _) Free = False

-- expand' :: [Char] -> [(Int, Int, Space)]
expand' = zipWith f zipper . map digitToInt
  where
    zipper = iterate (\(isFile, i) -> (not isFile, if not isFile then i + 1 else i)) (True, 0)
    f (isFile, fileIndex) n = replicate n (if isFile then File fileIndex else Free)

expand :: [Char] -> [(Int, Int, Space)]
expand = addIndexes . filter (/= []) . zipWith f zipper . map digitToInt
  where
    zipper = iterate (\(isFile, i) -> (not isFile, if not isFile then i + 1 else i)) (True, 0)
    f (isFile, fileIndex) n = replicate n (if isFile then File fileIndex else Free)
    -- I THINK I CAN JUST DO THE WORK OF addIndexes during `iterate` but I can't be bothered right now!
    addIndexes :: [[a]] -> [(Int, Int, a)]
    addIndexes = reverse . foldl folder []
      where
        folder :: [(Int, Int, a)] -> [a] -> [(Int, Int, a)]
        folder [] group = [(0, length group, head group)]
        folder ((i, j, prevGroup) : xss) group = (i + j, length group, head group) : (i, j, prevGroup) : xss

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