module Day6 where

data Race = Race {time :: Int, distance :: Int}

data Race' = Race' {time' :: Double, distance' :: Double}

coerceRace :: Race -> Race'
coerceRace (Race t d) = Race' (fromIntegral t) (fromIntegral d)

-- 0.01 secs
-- 741000
part1 :: Int
part1 = product . map getWins $ races

-- 9.47 secs
-- 38220708
-- I mean presumably you're supposed to solve a quadratic or something
part2 :: Int
part2 = getWins oneTrueRace

-- 0.01 secs
part1' :: Integer
part1' = product . map (getWins' . coerceRace) $ races

-- 0.00 secs
part2' :: Integer
part2' = getWins' . coerceRace $ oneTrueRace

races :: [Race]
races =
  [ Race 47 207,
    Race 84 1394,
    Race 74 1209,
    Race 67 1014
  ]

oneTrueRace :: Race
oneTrueRace = Race 47847467 207139412091014

getWins' :: Integral a => Race' -> a
getWins' (Race' raceTime dist) = upper - lower
  where
    upper = floor ((raceTime + sqrt (raceTime ^ 2 - 4 * dist)) / 2.0)
    lower = floor ((raceTime - sqrt (raceTime ^ 2 - 4 * dist)) / 2.0)

getWins :: Race -> Int
getWins (Race raceTime dist) = length . filter (> dist) . map getDistance $ [0 .. raceTime]
  where
    getDistance :: Int -> Int
    getDistance buttonHeldFor = buttonHeldFor * (raceTime - buttonHeldFor)