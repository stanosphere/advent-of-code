module Day8 where

import Data.Foldable (find)
import Data.List ((\\))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

type Display = S.Set Char

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input8.txt"
  let relevantParts = map getSuffix input
  let res = count (\x -> x `elem` [2, 3, 4, 7]) . concatMap (map length) $ relevantParts
  print res

part2 :: IO ()
part2 = do
  fullInput <- getLines "./fixtures/input8.txt"
  print . sum . mapMaybe decode $ fullInput

decode :: String -> Maybe Int
decode fullInputLine = do
  mapping <- findSolutionMapping fullInputLine
  return (getDigits mapping . getSuffix $ fullInputLine)

getSuffix :: String -> [String]
getSuffix = words . drop 2 . dropWhile (/= '|')

getDigits :: M.Map Char Char -> [String] -> Int
getDigits mapping = read . concatMap (show . (displayToNumber M.!) . S.fromList . map (mapping M.!))

findSolutionMapping :: String -> Maybe (M.Map Char Char)
findSolutionMapping fullInputLine = do
  prefix <- Just (words . takeWhile (/= '|') $ fullInputLine)
  candidateMaps <- getCandidateMaps prefix
  find (isValidSolution prefix) candidateMaps

getCandidateMaps :: [String] -> Maybe [M.Map Char Char]
getCandidateMaps inputLine = do
  a <- findMappingForA inputLine
  bd <- getCandidatesForBD inputLine
  cf <- getCandidatesForCF inputLine
  let eg = getCandidatesForEG (a : bd ++ cf)
  return
    [ M.unions [M.singleton a 'a', bd', cf', eg']
      | bd' <- mkChoices (bd, "bd"),
        cf' <- mkChoices (cf, "cf"),
        eg' <- mkChoices (eg, "eg")
    ]

isValidSolution :: [String] -> M.Map Char Char -> Bool
isValidSolution inputLine mapping =
  let displays = map (S.fromList . map (mapping M.!)) inputLine :: [Display]
      allowedDisplays = M.keys displayToNumber
   in all (`elem` allowedDisplays) displays

mkChoices :: (String, String) -> [M.Map Char Char]
mkChoices ([x1, x2], [y1, y2]) = [M.fromList [(x1, y1), (x2, y2)], M.fromList [(x1, y2), (x2, y1)]]

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

findMappingForA :: [String] -> Maybe Char
findMappingForA inputLine = do
  one <- find ((== 2) . length) inputLine
  seven <- find ((== 3) . length) inputLine
  return . head $ (seven \\ one)

getCandidatesForBD :: [String] -> Maybe [Char]
getCandidatesForBD inputLine = do
  one <- find ((== 2) . length) inputLine
  four <- find ((== 4) . length) inputLine
  return (four \\ one)

getCandidatesForCF :: [String] -> Maybe [Char]
getCandidatesForCF = find ((== 2) . length)

getCandidatesForEG :: [Char] -> [Char]
getCandidatesForEG knownCharacters = "abcdefg" \\ knownCharacters

displayToNumber :: M.Map Display Int
displayToNumber =
  M.fromList
    [ (S.fromList "abcefg", 0),
      (S.fromList "cf", 1),
      (S.fromList "acdeg", 2),
      (S.fromList "acdfg", 3),
      (S.fromList "bcdf", 4),
      (S.fromList "abdfg", 5),
      (S.fromList "abdefg", 6),
      (S.fromList "acf", 7),
      (S.fromList "abcdefg", 8),
      (S.fromList "abcdfg", 9)
    ]

invertMap :: (Ord b) => M.Map a b -> M.Map b a
invertMap = M.fromList . map (\(a, b) -> (b, a)) . M.toList

-- ugly parsing stuff below here
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInputLine :: String
toyInputLine = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"