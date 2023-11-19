module Day7 where

import Data.Char (isDigit)
import Data.Foldable ()
import Data.Function (on)
import Data.List (groupBy, nub, sortOn)
import Data.List.Split (splitOn)
import Data.Map qualified as M (Map, fromAscList, lookup, map)
import Data.Maybe (mapMaybe)

part1 :: IO ()
part1 = do
  lines <- getLines "./fixtures/input7.txt"
  let preProcessed = buildGraph lines
  let parentsMap = mkParentMap preProcessed
  let res = walk parentsMap ["shiny gold"] []
  print . length $ res

type ParentMap = M.Map String [String]

-- should be a set really
type Acc = [String]

type Candidates = [String]

walk :: ParentMap -> Candidates -> Acc -> Acc
walk mp cands acc =
  let xs = nub . concat . mapMaybe (`M.lookup` mp) $ cands
      res = if null xs then acc else walk mp xs (nub (acc ++ xs))
   in res

-- map leading to parents
mkParentMap :: [(String, String)] -> M.Map String [String]
mkParentMap = M.map (map fst) . scalaGroupBy snd

-- map leading to children

-- parsing stuff
buildGraph :: [String] -> [(String, String)]
buildGraph = concatMap ((\(from, tos) -> map (\to -> (from, to)) tos) . parseLine)
  where
    parseLine :: String -> (String, [String])
    parseLine s =
      let [container, contents] = splitOn " contain " s
       in (dropBags container, parseContents contents)

    parseContents :: String -> [String]
    parseContents = map parseContentItem . splitOn ", " . dropRight 1

    -- can either come as bag or bags plural
    -- NOTE: deosn't quite work for "contains no other bags but it shuldn't matter"
    parseContentItem :: String -> String
    parseContentItem item =
      let numberAndColour = dropBags item
          -- might need number later tbh
          -- number :: Int  = read . takeWhile isDigit $ numberAndColour
          colour = drop 1 . dropWhile isDigit $ numberAndColour
       in colour

    dropBags :: String -> String
    dropBags item = if last item == 's' then dropRight 5 item else dropRight 4 item

-- util functions
dropRight :: Int -> [a] -> [a]
dropRight n xs = take (length xs - n) xs

takeRight :: Int -> [a] -> [a]
takeRight n xs = drop (length xs - n) xs

scalaGroupBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
scalaGroupBy f = M.fromAscList . map (\x -> (f . head $ x, x)) . groupBy ((==) `on` f) . sortOn f

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)