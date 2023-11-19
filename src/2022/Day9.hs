module Day9 where

import Data.List (nub)

data Coords = Coords {x :: Int, y :: Int} deriving (Eq)

data Direction = U | D | L | R

data Command = Command {direction :: Direction, steps :: Int}

type Rope = [Coords]

part1 :: IO ()
part1 = solve 2 >>= print

part2 :: IO ()
part2 = solve 10 >>= print

solve :: Int -> IO Int
solve ropeLength =
  fmap
    ( length
        . nub
        . map last
        . scanl moveEntireRope (replicate ropeLength (Coords 0 0))
        . concatMap commandToDirections
        . map parseCommand
    )
    (getLines "./fixtures/input9.txt")

moveHead :: Coords -> Direction -> Coords
moveHead (Coords x y) U = Coords x (y + 1)
moveHead (Coords x y) D = Coords x (y - 1)
moveHead (Coords x y) L = Coords (x - 1) y
moveHead (Coords x y) R = Coords (x + 1) y

moveNext :: Coords -> Coords -> Coords
moveNext prev curr =
  if areTouching prev curr
    then curr
    else Coords ((x curr) + signum diffX) ((y curr) + signum diffY)
  where
    diffX = (x prev) - (x curr)
    diffY = (y prev) - (y curr)

moveEntireRope :: Rope -> Direction -> Rope
moveEntireRope (head : tail) d = scanl moveNext (moveHead head d) tail

-- could potentially use as a short circuiter
areTouching :: Coords -> Coords -> Bool
areTouching Coords {x = x1, y = y1} Coords {x = x2, y = y2} =
  abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

-- parsing
parseCommand :: String -> Command
parseCommand ('U' : ' ' : amount) = Command U (read amount)
parseCommand ('D' : ' ' : amount) = Command D (read amount)
parseCommand ('L' : ' ' : amount) = Command L (read amount)
parseCommand ('R' : ' ' : amount) = Command R (read amount)

commandToDirections :: Command -> [Direction]
commandToDirections Command {direction, steps} = replicate steps direction

-- boilerplate
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInstructions :: [String] =
  ["R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"]
