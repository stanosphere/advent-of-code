module Day5 where

import Control.Monad.Trans.State.Lazy
  ( State,
    execState,
    get,
    modify,
  )
import Data.Char (digitToInt)
import Data.List
  ( sortOn,
    transpose,
  )
import Data.List.Split (splitOn)
import qualified Data.Map as M

type From = Int

type To = Int

type Crates = String

data Command = CMD
  { amount :: Int,
    from :: From,
    to :: To
  }
  deriving (Show)

type CrateState = M.Map Int Crates

part1 :: IO ()
part1 = do
  (initialState, commands) <- parseInput "./fixtures/input5.txt"
  let res = execState (traverse moveCrates1 commands) initialState
  print . showAnswer $ res

part2 :: IO ()
part2 = do
  (initialState, commands) <- parseInput "./fixtures/input5.txt"
  let res = execState (traverse moveCrates2 commands) initialState
  print . showAnswer $ res

moveCrates1 :: Command -> State CrateState ()
moveCrates1 cmd = do
  cratesToMove <- getCrates cmd
  putCratesOneByOne cratesToMove cmd

moveCrates2 :: Command -> State CrateState ()
moveCrates2 cmd = do
  cratesToMove <- getCrates cmd
  putCratesAllAtOnce cratesToMove cmd

getCrates :: Command -> State CrateState Crates
getCrates cmd = do
  s <- get
  modify (M.update (Just . drop (amount cmd)) (from cmd))
  return . take (amount cmd) $ (s M.! from cmd)

putCratesOneByOne :: Crates -> Command -> State CrateState ()
putCratesOneByOne cratesToMove cmd =
  modify (M.update (Just . (reverse cratesToMove ++)) (to cmd))

putCratesAllAtOnce :: Crates -> Command -> State CrateState ()
putCratesAllAtOnce cratesToMove cmd =
  modify (M.update (Just . (cratesToMove ++)) (to cmd))

showAnswer :: CrateState -> String
showAnswer = map (head . snd) . sortOn fst . M.toList

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseInput :: FilePath -> IO (CrateState, [Command])
parseInput fp = do
  contents <- getLines fp
  let initState = mkInitialState contents
  let commands = mkCommands contents
  return (initState, commands)

mkCommands :: [String] -> [Command]
mkCommands = map commandFromString . drop 10

-- move 2 from 1 to 7
commandFromString :: String -> Command
commandFromString s =
  let x = drop 5 s
      [amount, rest] = splitOn " from " x
      [from, to] = splitOn " to " rest
   in CMD (read amount) (read from) (read to)

mkInitialState :: [String] -> CrateState
mkInitialState =
  M.fromList
    . map mkColumn
    . filterNot (== "")
    . filterNot (elem ']')
    . filterNot (elem '[')
    . map trim
    . transpose
    . take 9

mkColumn :: String -> (Int, String)
mkColumn s = case reverse s of
  int : rest -> (digitToInt int, reverse rest)

trim :: String -> String
trim = takeWhile (/= ' ') . dropWhile (== ' ')

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p = filter (not . p)
