module Day6 where

import Data.List (nub, tails)
import Data.List.Extra (splitOn)
import Data.Map qualified as M (Map, findWithDefault)
import Data.Set qualified as S (difference, fromList, size, toList)
import Data.Tree (Tree (Node), foldTree)
import Utils.Grouping (groupMap)

data Edge a = Edge {from :: a, to :: a} deriving (Show)

part1 :: IO ()
part1 = do
  inp <- getLines "./fixtures/input6.txt"
  print
    . sum
    . map ((\x -> x - 1) . length)
    . filter (not . null)
    . nub
    . concatMap tails
    . paths
    . buildTree
    . map parseLine
    $ inp

part2 :: IO ()
part2 = do
  inp <- getLines "./fixtures/input6.txt"
  let treePaths = paths . buildTree . map parseLine $ inp

  let myPath = reverse . head . filter (\x -> head x == "YOU") $ treePaths
  let santaPath = reverse . head . filter (\x -> head x == "SAN") $ treePaths

  let myPath' = dropWhile (`elem` santaPath) myPath
  let santaPath' = dropWhile (`elem` myPath) santaPath

  print myPath
  print santaPath
  print myPath'
  print (length santaPath' + length myPath' - 2)

-- I could just as well have written this to work on my Map representation
paths :: Tree a -> [[a]]
paths = appendPaths []
  where
    appendPaths :: [a] -> Tree a -> [[a]]
    appendPaths p (Node label []) = [label : p]
    appendPaths p (Node label children) = concatMap (appendPaths (label : p)) children

buildTree :: Ord a => [Edge a] -> Tree a
buildTree es =
  let myMap = groupMap from to es
   in buildTree' myMap (identifyRoot es)

-- could potentially delete the root entry from the map as we move down the tree...
buildTree' :: Ord a => M.Map a [a] -> a -> Tree a
buildTree' myMap root = Node root (map (buildTree' myMap) . M.findWithDefault [] root $ myMap)

identifyRoot :: Ord a => [Edge a] -> a
identifyRoot es =
  let froms = S.fromList . map from $ es
      tos = S.fromList . map to $ es
      rootCandidates = S.difference froms tos
   in if S.size rootCandidates == 1
        then head . S.toList $ rootCandidates
        else error "found more than one root!!"

parseLine :: String -> Edge String
parseLine s =
  let [from, to] = splitOn ")" s
   in Edge from to

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

-- using the drawTree function for the toy input we get the below. it's rather cool
{-

COM
\|  hmm for some reason my formatter inserts a "\" slightly breaking my beautiful output
`- B
   |
   +- C
   |  |
   |  `- D
   |     |
   |     +- E
   |     |  |
   |     |  +- F
   |     |  |
   |     |  `- J
   |     |     |
   |     |     `- K
   |     |        |
   |     |        `- L
   |     |
   |     `- I
   |
   `- G
      |
      `- H

-}

-- I don't understand this in the slightest so I shan't use it
-- it's from https://haskell-cafe.haskell.narkive.com/dqRM9vzR/how-to-get-all-the-paths-of-a-tree#post3
getAllpaths :: Tree a -> [[a]]
getAllpaths t = foldTree folder t []
  where
    folder :: a -> [[a] -> [[a]]] -> [a] -> [[a]]
    folder a fs p = let p' = p ++ [a] in if null fs then [p'] else concatMap ($ p') fs