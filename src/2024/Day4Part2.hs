module Day4Part2 where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Coord = (Int, Int)

type Grid = M.Map Coord Char

-- plan
-- very similar to part1 tbh, just the checking is a little different

part2 :: IO Int
part2 = countAllXmasOccurrences . toGrid <$> getInput

countAllXmasOccurrences :: Grid -> Int
countAllXmasOccurrences grid =
  length
    . filter (isXmas grid)
    . findAPositions
    $ grid

isXmas :: Grid -> Coord -> Bool
isXmas grid (x, y) = thisCombo `elem` validCombos
  where
    lookupFn c = fromMaybe ' ' . M.lookup c $ grid
    tl = lookupFn (x - 1, y - 1)
    tr = lookupFn (x + 1, y - 1)
    bl = lookupFn (x - 1, y + 1)
    br = lookupFn (x + 1, y + 1)
    thisCombo =
      [ [tl, ' ', tr],
        [' ', 'A', ' '],
        [bl, ' ', br]
      ]
    validCombos =
      [ [ "M S",
          " A ",
          "M S"
        ],
        [ "M M",
          " A ",
          "S S"
        ],
        [ "S M",
          " A ",
          "S M"
        ],
        [ "S S",
          " A ",
          "M M"
        ]
      ]

findAPositions :: Grid -> [Coord]
findAPositions = M.keys . M.filter (== 'A')

-- I could probably have done this as a list comp couldn't I...
stepFunctions :: [Coord -> Coord]
stepFunctions =
  [ \(x, y) -> (x - 1, y - 1),
    \(x, y) -> (x - 1, y),
    \(x, y) -> (x - 1, y + 1),
    \(x, y) -> (x, y - 1),
    \(x, y) -> (x, y + 1),
    \(x, y) -> (x + 1, y - 1),
    \(x, y) -> (x + 1, y),
    \(x, y) -> (x + 1, y + 1)
  ]

toGrid :: [String] -> Grid
toGrid grid =
  M.fromList
    [ ((i, j), char)
      | (j, row) <- zipWithIndex grid,
        (i, char) <- zipWithIndex row,
        char `elem` "MAS"
    ]
  where
    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input4.txt"