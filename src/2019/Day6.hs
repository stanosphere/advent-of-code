module Day6 where

part1 = do
  inp <- getLines "./fixtures/input6.txt"
  traverse print inp

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
