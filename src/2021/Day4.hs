module Day4 where

import Data.Foldable
import Data.List (group, transpose)
import Data.Text qualified as T

type HasBeenCalled = Bool

-- 5 * 5 bingo board
type Board = [[(Int, HasBeenCalled)]]

-- get board that wins first
part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input4.txt"
  let numbersCalled = parseNumbersToCall input
  let bingos = parseBingoBoards input
  let winner = findWinner numbersCalled bingos
  print winner
  print . getScoreForWinningBoard $ winner

-- get board that wins last
part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input4.txt"
  let numbersCalled = parseNumbersToCall input
  let bingos = parseBingoBoards input
  let lastWinner = findLastWinner numbersCalled bingos
  print . getScoreForWinningBoard $ lastWinner

findWinner :: [Int] -> [Board] -> (Int, Board)
findWinner numbersToCall boards =
  let numberCalled = head numbersToCall
      nextBoards = map (updateBoardState numberCalled) boards
      maybeWinner = find hasWon nextBoards
   in case maybeWinner of
        Nothing -> findWinner (tail numbersToCall) nextBoards
        Just winner -> (numberCalled, winner)

findLastWinner :: [Int] -> [Board] -> (Int, Board)
findLastWinner numbersToCall boards =
  let numberCalled = head numbersToCall
      nextBoards = map (updateBoardState numberCalled) boards
      boardsNotYetWon = filter (not . hasWon) nextBoards
   in if null boardsNotYetWon
        then (numberCalled, head nextBoards)
        else findLastWinner (tail numbersToCall) boardsNotYetWon

updateBoardState :: Int -> Board -> Board
updateBoardState numberCalled = nestedMap (\(number, hasBeenCalled) -> if number == numberCalled then (number, True) else (number, hasBeenCalled))

nestedMap :: (a -> b) -> [[a]] -> [[b]]
nestedMap f = map (map f)

hasWon :: Board -> Bool
hasWon board = hasWinningRow board || hasWinningColumn board

hasWinningRow :: Board -> Bool
hasWinningRow = any (all snd)

hasWinningColumn :: Board -> Bool
hasWinningColumn = hasWinningRow . transpose

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

mkBoard :: [String] -> Board
mkBoard = map (map (\x -> (read x :: Int, False)) . words)

getScoreForWinningBoard :: (Int, Board) -> Int
getScoreForWinningBoard (finalNumberCalled, board) = finalNumberCalled * (sum . concat . nestedMap (\(x, hbc) -> if hbc then 0 else x) $ board)

parseNumbersToCall :: [String] -> [Int]
parseNumbersToCall = map (\t -> read t :: Int) . map T.unpack . T.splitOn (T.pack ",") . T.pack . head

parseBingoBoards :: [String] -> [Board]
parseBingoBoards = map mkBoard . map tail . chunk 6 . tail

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)