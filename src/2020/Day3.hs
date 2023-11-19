module Day3 where

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input3.txt"
  print . go contents . getCoords $ (3, 1)

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input3.txt"
  print . product . map (go contents . getCoords) $ [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

go :: [String] -> [(Int, Int)] -> Int
go contents = length . filter isTree . map getSquareType
  where
    getSquareType :: (Int, Int) -> Char
    getSquareType (x, y) = (contents !! y) !! (x `mod` 31)

    isTree :: Char -> Bool
    isTree = (== '#')

getCoords :: (Int, Int) -> [(Int, Int)]
getCoords (right, down) =
  let rights = map (right *) [0 ..]
      downs = takeWhile (< 323) . map (down *) $ [0 ..]
   in zip rights downs