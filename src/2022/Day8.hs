module Day8 where

import Data.Char (digitToInt)
import Data.List (transpose)
import Data.Map qualified as M
  ( Map,
    alter,
    elems,
    empty,
    insert,
    size,
  )

type TreeHeight = Int

type Coords = (Int, Int)

type TreeGrid = [[(Coords, TreeHeight)]]

type RowState = (TreeHeight, VisibilityMap)

data Visibility = Visible | Hidden deriving (Show)

type VisibilityMap = M.Map Coords Visibility

type ScenicScore = Int

type ScenicMap = M.Map Coords ScenicScore

part1 :: IO ()
part1 =
  getLines "./fixtures/input8.txt"
    >>= (print . M.size . processAllGrids . parseInput)

part2 :: IO ()
part2 =
  getLines "./fixtures/input8.txt"
    >>= (print . maximum . M.elems . processAllGrids' . parseInput)

processRow :: VisibilityMap -> [(Coords, TreeHeight)] -> VisibilityMap
processRow tm xs =
  let initial = (-1, tm)
      res =
        foldl
          ( \(tallest, thisTm) (coords, height) ->
              if height > tallest
                then (height, M.insert coords Visible thisTm)
                else (tallest, thisTm)
          )
          initial
          xs
   in snd res

processAllGrids :: TreeGrid -> VisibilityMap
processAllGrids = foldl processGrid M.empty . getVariants

processGrid :: VisibilityMap -> TreeGrid -> VisibilityMap
processGrid = foldl processRow

parseInput :: [[Char]] -> TreeGrid
parseInput =
  zipWith
    (\i row -> zipWith (\j height -> ((i, j), digitToInt height)) [0 ..] row)
    [0 ..]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInput :: [String]
toyInput = ["30373", "25512", "65332", "33549", "35390"]

-- how many trees can I see?
getScenicScoresForRow :: [(Coords, TreeHeight)] -> [(Coords, ScenicScore)]
getScenicScoresForRow xs =
  let withIndex = zip [0 ..] xs
      justHeights = map snd xs
      res =
        map
          ( \(i, (coords, height)) ->
              ( coords,
                length . takeWhileOneMore (< height) . drop (i + 1) $ justHeights
              )
          )
          withIndex
   in res

processRow' :: ScenicMap -> [(Coords, TreeHeight)] -> ScenicMap
processRow' scenicMap =
  foldl
    (\acc (coords, score) -> M.alter (updateScore score) coords acc)
    scenicMap
    . getScenicScoresForRow
  where
    updateScore :: ScenicScore -> Maybe ScenicScore -> Maybe ScenicScore
    updateScore score (Just scoreInMap) = Just (scoreInMap * score)
    updateScore score Nothing = Just score

processGrid' :: ScenicMap -> TreeGrid -> ScenicMap
processGrid' = foldl processRow'

processAllGrids' :: TreeGrid -> ScenicMap
processAllGrids' = foldl processGrid' M.empty . getVariants

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

-- effectively allows us to go up,dow, right, and left
getVariants :: [[a]] -> [[[a]]]
getVariants tg =
  let tg' = map reverse tg
      tg'' = transpose tg'
      tg''' = map reverse tg''
   in [tg, tg', tg'', tg''']
