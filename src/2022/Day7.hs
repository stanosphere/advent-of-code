module Day7 where

import Data.Foldable (traverse_)
import Data.List (inits)
import Data.Map qualified as M
  ( Map,
    alter,
    elems,
    empty,
  )
import Data.Maybe (mapMaybe)

type CurrentLocation = [String]

type DirectorySizeMap = M.Map [String] Integer

data SystemState = S
  { loc :: CurrentLocation,
    sizes :: DirectorySizeMap
  }
  deriving (Show)

data File = File
  { fileName :: String,
    size :: Integer
  }
  deriving (Show)

newtype Directory = Directory {dirName :: String} deriving (Show)

data CD = Down {subDir :: String} | Up | Top deriving (Show)

data CMD = LS | CD {cd :: CD} deriving (Show)

data TerminalLine = Command CMD | F File | D Directory deriving (Show)

-- consider something like cd ffhwwg
-- want to refer to a directory by its full name!

part1 :: IO ()
part1 =
  getLines "./fixtures/input7.txt"
    >>= ( print
            . sum
            . filter (<= 100000)
            . M.elems
            . sizes
            . processAllTerminalLines
            . map parseLine
        )

part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input7.txt"
  let terminalLines = map parseLine input
  let sizeWeNeedToRemove = getTotalSize terminalLines - 40000000
  print
    . minimum
    . filter (>= sizeWeNeedToRemove)
    . M.elems
    . sizes
    . processAllTerminalLines
    $ terminalLines

-- work!
getTotalSize :: [TerminalLine] -> Integer
getTotalSize = sum . map size . mapMaybe predicate
  where
    predicate :: TerminalLine -> Maybe File
    predicate (F file) = Just file
    predicate _ = Nothing

processAllTerminalLines :: [TerminalLine] -> SystemState
processAllTerminalLines =
  foldl processTerminalLine (S {loc = [], sizes = M.empty})

processTerminalLine :: SystemState -> TerminalLine -> SystemState
processTerminalLine state (F file) = updateDirectorySizes state file
processTerminalLine S {loc, sizes} (Command (CD (Down subDir))) =
  S (loc ++ [subDir]) sizes
processTerminalLine S {loc, sizes} (Command (CD Up)) = S (init loc) sizes
processTerminalLine S {loc, sizes} (Command (CD Top)) = S ["/"] sizes
processTerminalLine state (D _) = state
processTerminalLine state (Command LS) = state

-- this file is in many directories so we must update all of them
updateDirectorySizes :: SystemState -> File -> SystemState
updateDirectorySizes S {loc, sizes} File {fileName, size} =
  S
    { loc,
      sizes =
        foldl
          (flip (M.alter (adjustMentFuntion size)))
          sizes
          (drop 1 . inits $ loc)
    }
  where
    adjustMentFuntion :: Integer -> Maybe Integer -> Maybe Integer
    adjustMentFuntion i Nothing = Just i
    adjustMentFuntion i (Just j) = Just (i + j)

-- parsing
parseLine :: String -> TerminalLine
parseLine s
  | head s == '$' = Command (parseCommand s)
  | take 3 s == "dir" = D (Directory (drop 4 s))
  | otherwise = F . parseFileListing $ s

parseFileListing :: String -> File
parseFileListing s = File b (read a) where [a, b] = words s

parseCommand :: String -> CMD
parseCommand "$ ls" = LS
parseCommand s = CD . parseCD $ s

parseCD :: String -> CD
parseCD "$ cd .." = Up
parseCD "$ cd /" = Top
parseCD s = Down . drop 5 $ s

-- boilerplate
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInput :: [String]
toyInput =
  [ "$ cd /",
    "$ ls",
    "dir a",
    "14848514 b.txt",
    "8504156 c.dat",
    "dir d",
    "$ cd a",
    "$ ls",
    "dir e",
    "29116 f",
    "2557 g",
    "62596 h.lst",
    "$ cd e",
    "$ ls",
    "584 i",
    "$ cd ..",
    "$ cd ..",
    "$ cd d",
    "$ ls",
    "4060174 j",
    "8033020 d.log",
    "5626152 d.ext",
    "7214296 k"
  ]
