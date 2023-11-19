module Day10 where

import Data.Foldable (traverse_)
import Data.List.Split (chunksOf)

data Command = Add Int | NoOp deriving (Show)

newtype State = S {x :: Int} deriving (Show)

part1 :: IO ()
part1 =
  getLines "./fixtures/input10.txt"
    >>= ( print
            . sum
            . map (\(cycle, S x) -> cycle * x)
            . filter (shouldKeep . fst)
            . zip [1 ..]
            . scanl updateState (S 1)
            . concatMap parseCommand
        )

part2 :: IO ()
part2 =
  getLines "./fixtures/input10.txt"
    >>= ( traverse_ print
            . take 6
            . chunksOf 40
            . zipWith charToPrint [0 ..]
            . scanl updateState (S 1)
            . concatMap parseCommand
        )

-- part1 functions
shouldKeep :: Int -> Bool
shouldKeep i = i `elem` [20, 60 .. 220]

updateState :: State -> Command -> State
updateState s NoOp = s
updateState (S x) (Add y) = S (x + y)

-- part2 functions
charToPrint :: Int -> State -> Char
charToPrint i (S s) = if rem i 40 `elem` [s - 1, s, s + 1] then '#' else ' '

-- parsing
parseCommand :: String -> [Command]
parseCommand "noop" = [NoOp]
parseCommand s = [NoOp, Add . read . drop 5 $ s]

-- boilerplate
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
