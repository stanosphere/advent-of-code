module Day9Part2 where

import Data.Char (digitToInt)
import Data.Foldable (find)
import Data.List (sortOn)
import Data.Maybe (catMaybes, isJust)

part2 :: IO Integer
part2 = checksum . zip [0 ..] . unpack . reOrder . expand <$> getInput

checksum :: [(Integer, Space)] -> Integer
checksum = sum . map f
  where
    f (_, Free) = 0
    f (i, File j) = i * j

-- could just use Maybe tbh
data Space = Free | File Integer deriving (Show, Eq)

unpack :: TransformState -> [Space]
unpack (TS ws xs ys zs) =
  concatMap (\(_, n, sp) -> replicate n sp) . sortOn (\(i, _, _) -> i) $
    (ws ++ xs ++ ys ++ zs)

-- reOrder :: [(Int, Int, Space)] -> String
reOrder =
  last
    . catMaybes
    . takeWhile isJust
    . iterate (wrap transform)
    . Just
    . mkInitialState

mkInitialState :: [(Int, Int, Space)] -> TransformState
mkInitialState xs = TS holes [] (reverse values) []
  where
    holes = filter (\(_, _, space) -> space == Free) xs
    values = filter (\(_, _, space) -> space /= Free) xs

transform :: TransformState -> Maybe TransformState
transform (TS _ _ [] _) = Nothing
transform (TS fillableHoles unFillableHoles ((fileIndex, fileSize, fileId) : otherFiles) newValues) =
  case find (\(holeIndex, holeSize, _) -> holeSize >= fileSize && holeIndex < fileIndex) fillableHoles of
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
    before = takeWhile (\(i, _, _) -> i < holeIndex) holeList
    after = dropWhile (\(i, _, _) -> i <= holeIndex) holeList

-- look for holes, if you can find one then great! fill her up
-- if not then just put the file in the newValues list, it has basically been left alone

data TransformState = TS
  { _fillableHoles :: [(Int, Int, Space)], -- holes don't really need the space in them I guess
    _unFillableHoles :: [(Int, Int, Space)], -- don't strictly need notion of un fillable holes but it reduces the search space in future iterations
    _reversedFileList :: [(Int, Int, Space)],
    _newValues :: [(Int, Int, Space)]
  }
  deriving (Show)

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