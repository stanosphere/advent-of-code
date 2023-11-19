module Day11 where

import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import Data.Map
  ( Map,
    adjust,
    elems,
    fromList,
    (!),
  )

data Monkey = Monkey
  { items :: [Int],
    operation :: Int -> Int,
    test :: Int -> Int,
    inspectionCount :: Int
  }

type MonkeyState = Map Int Monkey

instance Show Monkey where
  show (Monkey a _ _ b) = show a ++ ";" ++ show b

part1 :: IO Int
part1 = solve (`div` 3) 20

part2 :: IO Int
part2 = solve moduloCommonFactor 10000

solve :: (Int -> Int) -> Int -> IO Int
solve worryReducer rounds =
  fmap
    ( multiplyMostActive
        . doRounds rounds
        . fromList
        . zip [0 ..]
        . map (parseMonkey worryReducer)
        . map (take 6)
        . chunksOf 7
    )
    . getLines
    $ "./fixtures/input11.txt"

multiplyMostActive :: MonkeyState -> Int
multiplyMostActive ms =
  let [a, b] = getMostActive ms in (inspectionCount a) * (inspectionCount b)

getMostActive :: MonkeyState -> [Monkey]
getMostActive ms = take 2 . sortOn ((* (-1)) . inspectionCount) . elems $ ms

doRounds :: Int -> MonkeyState -> MonkeyState
doRounds i = last . take (i + 1) . iterate doRound

doRound :: MonkeyState -> MonkeyState
doRound ms = foldl doMonkeyTurn ms [0 .. 7]

doMonkeyTurn :: MonkeyState -> Int -> MonkeyState
doMonkeyTurn ms id =
  last
    . takeWhileOneMore (\ms' -> items (ms' ! id) /= [])
    . iterate (throwItem id)
    $ ms

throwItem :: Int -> MonkeyState -> MonkeyState
throwItem id ms =
  if null . items $ monkey
    then ms
    else removeItem . addItem . incrementCount $ ms
  where
    monkey = ms ! id
    item = operation monkey . head . items $ monkey
    reciever = test monkey item
    incrementCount = adjust (updateMonkeyInspectionCount (+ 1)) id
    removeItem = adjust (updateMonkeyItems tail) id
    addItem = adjust (updateMonkeyItems (\xs -> xs ++ [item])) reciever

updateMonkeyItems :: ([Int] -> [Int]) -> (Monkey -> Monkey)
updateMonkeyItems f monkey = monkey {items = f . items $ monkey}

updateMonkeyInspectionCount :: (Int -> Int) -> (Monkey -> Monkey)
updateMonkeyInspectionCount f monkey =
  monkey {inspectionCount = f . inspectionCount $ monkey}

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

-- parsing
parseMonkey :: (Int -> Int) -> [String] -> Monkey
parseMonkey worryReducer [_, itemsLine, operationLine, testLine, trueLine, falseLine] =
  Monkey
    { items = read ("[" ++ drop 18 itemsLine ++ "]"),
      operation = worryReducer . mkOperation operationLine,
      test = mkTest (testLine, trueLine, falseLine),
      inspectionCount = 0
    }

moduloCommonFactor :: Int -> Int
moduloCommonFactor = (`rem` 9699690)

mkOperation :: String -> (Int -> Int)
mkOperation s
  | s == "  Operation: new = old * old" = \x -> x * x
  | take 25 s == "  Operation: new = old + " = \x -> (read . drop 25 $ s) + x
  | take 25 s == "  Operation: new = old * " = \x -> (read . drop 25 $ s) * x
  | otherwise = undefined

mkTest :: (String, String, String) -> (Int -> Int)
mkTest (testLine, trueLine, falseLine) =
  let n :: Int = read . drop 21 $ testLine
   in ( \x ->
          if x `rem` n == 0
            then read . drop 29 $ trueLine
            else read . drop 30 $ falseLine
      )

-- boilerplate
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
