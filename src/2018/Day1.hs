module Day1 where

import Data.List (find)
import qualified Data.Set as S
  ( Set,
    empty,
    insert,
    member,
  )

part1 :: IO ()
part1 = do
  freqs <- input
  print . sum $ freqs

part2 :: IO ()
part2 = do
  freqs <- input
  print . findFreqTwice $ freqs

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseNum :: String -> Int
parseNum ('+' : rest) = read rest
parseNum ('-' : rest) = (-1) * read rest
parseNum _ = undefined

input :: IO [Int]
input = do
  contents <- getLines "./fixtures/input1.txt"
  return . map parseNum $ contents

findFreqTwice :: [Int] -> Maybe Int
findFreqTwice = fmap freq . find hasBeenSeenBefore . getStates . cycle

data State = State {freqsFound :: S.Set Int, hasBeenSeenBefore :: Bool, freq :: Int} deriving (Show)

getStates :: [Int] -> [State]
getStates = scanl evolveState initialState

initialState :: State
initialState = State (S.insert 0 S.empty) False 0

evolveState :: State -> Int -> State
evolveState s i =
  let nextFreq = freq s + i
   in if S.member nextFreq (freqsFound s)
        then State (freqsFound s) True nextFreq
        else State (S.insert nextFreq (freqsFound s)) False nextFreq