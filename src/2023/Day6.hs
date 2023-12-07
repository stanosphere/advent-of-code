module Day6 where

data Race = Race {time :: Int, distance :: Int}

-- 0.01 secs
-- 741000
part1 :: Int
part1 = product . map getWins $ races

-- 9.47 secs
-- 38220708
-- I mean presumably you're supposed to solve a quadratic or something
part2 :: Int
part2 = getWins oneTrueRace

races :: [Race]
races =
  [ Race 47 207,
    Race 84 1394,
    Race 74 1209,
    Race 67 1014
  ]

oneTrueRace :: Race
oneTrueRace = Race 47847467 207139412091014

getWins :: Race -> Int
getWins (Race raceTime dist) = length . filter (> dist) . map getDistance $ [0 .. raceTime]
  where
    getDistance :: Int -> Int
    getDistance buttonHeldFor = buttonHeldFor * (raceTime - buttonHeldFor)