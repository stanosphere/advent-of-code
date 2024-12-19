module Day17 where

import Data.Foldable (traverse_)
import Data.List
  ( foldl1,
    groupBy,
  )
import qualified Data.Map as M
  ( Map,
  )
import qualified Data.Set as S
  ( Set,
    empty,
    fromList,
    intersection,
    map,
    toList,
    union,
  )

data Point = P
  { x :: Int,
    y :: Int
  }
  deriving (Eq, Ord, Show)

type AllOccupiedPoints = S.Set Point

type HighestOccupation = M.Map Int Int -- keep around for now

type Shape = S.Set Point

data Rock = Long | Plus | Corner | Tall | Square deriving (Show)

data SystemState = SS
  { occupiedPoints :: AllOccupiedPoints,
    shape :: Shape,
    rocksFallen :: Int,
    highestPoint :: Int,
    jets :: [Char],
    rocks :: [Rock]
  }

-- instance Show SystemState where
--   show :: SystemState -> String
--   show (SS occupiedPoints shape rocksFallen highestPoint jets rocks) = foldl1
--     (\a b -> a ++ "\n" ++ b)
--     [ "rocks fallen: " ++ show rocksFallen
--     , "highest point: " ++ show highestPoint
--     , "next 10 rocks: " ++ (show . take 10 $ rocks)
--     , "next 10 jets: " ++ take 10 jets
--     ]

instance Show SystemState where
  show (SS occupiedPoints shape rocksFallen highestPoint jets rocks) =
    foldl1
      (\a b -> a ++ "\n" ++ b)
      (showRockPattern highestPoint shape occupiedPoints)

showRockPattern :: Int -> Shape -> AllOccupiedPoints -> [String]
showRockPattern highestOccupied shape points =
  reverse
    ( "-------"
        : [ [toPrintable shape points (P x y) | x <- [0 .. 6]]
            | y <- [0 .. (highestOccupied + 7)]
          ]
    )
  where
    toPrintable :: Shape -> AllOccupiedPoints -> Point -> Char
    toPrintable shape points point =
      if point `elem` shape then '@' else if point `elem` points then '#' else '.'

part1 :: IO ()
part1 = do
  input <- realInput
  let initalState =
        SS
          (S.fromList . map (\x -> P x 0) $ [0 .. 6])
          (spawnShape 0 Long)
          0
          0
          (cycle input)
          (cycle [Plus, Corner, Tall, Square, Long])
      states = iterate singleStep initalState
  print
    . highestPoint
    . last
    . takeWhile (\x -> rocksFallen x < 2023)
    $ states

singleStep :: SystemState -> SystemState
singleStep = moveShapeDownOrSpawnNewShape . shiftShapeHorizontally'

moveShapeDownOrSpawnNewShape :: SystemState -> SystemState
moveShapeDownOrSpawnNewShape (SS occupiedPoints shape rocksFallen highestPoint jets rocks) =
  if canMoveDown occupiedPoints shape
    then
      SS
        occupiedPoints
        (shiftShapeDown shape)
        rocksFallen
        highestPoint
        jets
        rocks
    else
      SS
        (occupiedPoints `S.union` shape)
        (spawnShape newHighPoint (head rocks))
        (rocksFallen + 1)
        newHighPoint
        jets
        (tail rocks)
  where
    newHighPoint = getNewHighPoint highestPoint shape

getNewHighPoint :: Int -> Shape -> Int
getNewHighPoint prevHighest shape =
  let shapeYCoords = S.toList . S.map y $ shape
   in maximum (prevHighest : shapeYCoords)

canMoveDown :: AllOccupiedPoints -> Shape -> Bool
canMoveDown points = doesNotHitOtherShape points . shiftShapeDown

shiftShapeDown :: Shape -> Shape
shiftShapeDown = S.map (\(P x y) -> P x (y - 1))

shiftShapeHorizontally' :: SystemState -> SystemState
shiftShapeHorizontally' (SS occupiedPoints shape rocksFallen highestPoint jets rocks) =
  SS
    occupiedPoints
    (shiftShapeHorizontally (head jets) occupiedPoints shape)
    rocksFallen
    highestPoint
    (tail jets)
    rocks

shiftShapeHorizontally :: Char -> AllOccupiedPoints -> Shape -> Shape
shiftShapeHorizontally '<' points shape =
  if canMoveLeft points shape then shiftShapeLeft shape else shape
shiftShapeHorizontally '>' points shape =
  if canMoveRight points shape then shiftShapeRight shape else shape

shiftShapeLeft :: Shape -> Shape
shiftShapeLeft = S.map (\(P x y) -> P (x - 1) y)

shiftShapeRight :: Shape -> Shape
shiftShapeRight = S.map (\(P x y) -> P (x + 1) y)

canMoveLeft :: AllOccupiedPoints -> Shape -> Bool
canMoveLeft points shape =
  let leftShifted = shiftShapeLeft shape
   in doesNotHitLeftWall leftShifted && doesNotHitOtherShape points leftShifted

canMoveRight :: AllOccupiedPoints -> Shape -> Bool
canMoveRight points shape =
  let rightShifted = shiftShapeRight shape
   in doesNotHitRightWall rightShifted
        && doesNotHitOtherShape points rightShifted

doesNotHitOtherShape :: AllOccupiedPoints -> Shape -> Bool
doesNotHitOtherShape points shape = (points `S.intersection` shape) == S.empty

doesNotHitRightWall :: Shape -> Bool
doesNotHitRightWall = all (\(P x _) -> x <= 6)

doesNotHitLeftWall :: Shape -> Bool
doesNotHitLeftWall = all (\(P x _) -> x >= 0)

spawnShapeAtOrigin :: Rock -> [Point]
spawnShapeAtOrigin Long = [P 2 4, P 3 4, P 4 4, P 5 4]
spawnShapeAtOrigin Plus = [P 2 5, P 3 4, P 3 5, P 3 6, P 4 5]
spawnShapeAtOrigin Corner = [P 2 4, P 3 4, P 4 4, P 4 5, P 4 6]
spawnShapeAtOrigin Tall = [P 2 4, P 2 5, P 2 6, P 2 7]
spawnShapeAtOrigin Square = [P 2 4, P 2 5, P 3 4, P 3 5]

spawnShape :: Int -> Rock -> Shape
spawnShape yMax =
  S.fromList . map (\(P x y) -> P x (y + yMax)) . spawnShapeAtOrigin

-- I think we only really care about the highest occupation on each level
-- hmm not quite because you could have like a really long drop that then maybe opens up further down
-- so I think you do need to remember lots of stuff
-- but I think if you end up filling up a row completely then you can forget about everything below it!!
-- so for now I might just try to do the simple version where I remmber everything...

-- could use replicate or something to make this an infinite set of instructions
toyInput :: String
toyInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

realInput :: IO String
realInput = fmap head . getLines $ "./fixtures/input17.txt"

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

groupOn :: (Eq k) => (a -> k) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)
