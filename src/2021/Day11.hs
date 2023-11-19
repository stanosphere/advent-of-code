module Day11 where

import Data.Char (digitToInt)
import Data.List (findIndex)
import Data.Map qualified as M

type Coords = (Int, Int)

type Energy = Int

type NumberOfFlashes = Int

type Grid = M.Map Coords Octopus

data FlashState = NotFlashed | JustFlashed | AlreadyFlashed deriving (Show, Eq)

data Octopus = Octopus {energy :: Energy, flashState :: FlashState} deriving (Show)

part1 :: NumberOfFlashes
part1 = snd . performNSteps 100 $ (toCoordinateMap realInput, 0)

part2 = findIndex haveSynced . iterate step' $ toCoordinateMap realInput

haveSynced :: Grid -> Bool
haveSynced = all ((== 0) . energy . snd) . M.toList

performNSteps :: Int -> (Grid, NumberOfFlashes) -> (Grid, NumberOfFlashes)
performNSteps 0 x = x
performNSteps n (g, flashCount) = let (g', x) = step g in performNSteps (n - 1) (g', flashCount + x)

step :: Grid -> (Grid, NumberOfFlashes)
step g =
  let g' = propegateFlashes . increaseAllByOne $ g
   in (resetFlashedOctopusesToZero g', getTotalFlashesForThisStep g')

step' :: Grid -> Grid
step' = resetFlashedOctopusesToZero . propegateFlashes . increaseAllByOne

getTotalFlashesForThisStep :: Grid -> NumberOfFlashes
getTotalFlashesForThisStep = count (\o -> flashState o `elem` [JustFlashed, AlreadyFlashed])

count :: Ord k => (v -> Bool) -> M.Map k v -> Int
count p = M.size . M.filter p

resetFlashedOctopusesToZero :: Grid -> Grid
resetFlashedOctopusesToZero = M.map (\o -> if energy o > 9 then Octopus 0 NotFlashed else o)

propegateFlashes :: Grid -> Grid
propegateFlashes g =
  let justFlashed = getJustFlashedCoords g
      g' = setJustFlashedToAlreadyFlashed g justFlashed
      allNeighbors = concatMap getNeighbors justFlashed
   in if null justFlashed then g else propegateFlashes (foldl updateNeighbor g' allNeighbors)

updateNeighbor :: Grid -> Coords -> Grid
updateNeighbor g c = M.update (Just . updateOctopus) c g

updateOctopus :: Octopus -> Octopus
updateOctopus (Octopus energy NotFlashed) = Octopus (energy + 1) (if energy == 9 then JustFlashed else NotFlashed)
updateOctopus (Octopus energy JustFlashed) = Octopus (energy + 1) JustFlashed
updateOctopus (Octopus energy AlreadyFlashed) = Octopus (energy + 1) AlreadyFlashed

getJustFlashedCoords :: Grid -> [Coords]
getJustFlashedCoords = M.keys . M.filter ((== JustFlashed) . flashState)

increaseAllByOne :: Grid -> Grid
increaseAllByOne = M.map (\o -> Octopus (energy o + 1) (if energy o == 9 then JustFlashed else NotFlashed))

setJustFlashedToAlreadyFlashed :: Grid -> [Coords] -> Grid
setJustFlashedToAlreadyFlashed = foldl setFlashState
  where
    setFlashState g c = M.update (\o -> Just (Octopus (energy o) AlreadyFlashed)) c g

getNeighbors :: Coords -> [Coords]
getNeighbors (x, y) = [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]

-- parsing stuff
toCoordinateMap :: [[Int]] -> Grid
toCoordinateMap grid =
  M.fromList
    [ ((i, j), Octopus elem NotFlashed)
      | (i, row) <- zipWithIndex grid,
        (j, elem) <- zipWithIndex row
    ]

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

-- inputs
realInput :: [[Int]]
realInput =
  map
    (map digitToInt)
    [ "4658137637",
      "3277874355",
      "4525611183",
      "3128125888",
      "8734832838",
      "4175463257",
      "8321423552",
      "4832145253",
      "8286834851",
      "4885323138"
    ]

smallToyInput :: [[Int]]
smallToyInput =
  map
    (map digitToInt)
    [ "11111",
      "19991",
      "19191",
      "19991",
      "11111"
    ]

bigToyInput :: [[Int]]
bigToyInput =
  map
    (map digitToInt)
    [ "5483143223",
      "2745854711",
      "5264556173",
      "6141336146",
      "6357385478",
      "4167524645",
      "2176841721",
      "6882881134",
      "4846848554",
      "5283751526"
    ]
