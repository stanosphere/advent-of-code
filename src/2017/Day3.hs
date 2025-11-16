module Day3 where

part1 :: IO Int
part1 = do
  number <- getInput
  let sr = (\x -> if even x then x + 1 else x + 2) . floor . sqrt . fromIntegral $ number
  let dist = (sr * sr) - number
  let n = (sr + 1) `div` 2
  let sideLength = 2 * (n - 1)
  let distanceFromCorner = dist `rem` sideLength
  let distanceFromOrigin = abs ((n - 1) - distanceFromCorner) + (n - 1)
  return distanceFromOrigin

-- 37  36  35  34  33  32  31
-- 38  17  16  15  14  13  30
-- 39  18   5   4   3  12  29
-- 40  19   6   1   2  11  28
-- 41  20   7   8   9  10  27
-- 42  21  22  23  24  25  26
-- 43  44  45  46  47  48  49

-- 8,16,24

-- ok so find the next odd sqrt to work out which square you're in
-- do do this add 1 and divide by 2
-- so if you have 23 then the next odd sqrt is 5
-- (5 + 1) / 2 == 3 so you're in the third square
-- great stuff
-- n = (sr + 1) / 2
-- now 2nd square there are 8 numbers, in the 3rd there are 16, in the 4th there are 24
-- so in the nth square there are 8 * (n - 1) numbers
-- the nth square has a side length of 2 * (n - 1)
-- ok now to work out how far we are from the centre
-- well part of this is super easy actually - one of the coords is just gonna be n - 1
-- the other I think we can work out with some maths (obvs)
-- so first let's subtract x from sr ^ 2
-- that tells us how far away from a corner we are
-- if it's zero we're bang on a corner, if not things get a little fiddly
-- I think we can `rem` sideLength it because symmetry
-- then given the side length work out exactly where we are

getInput :: IO Int
getInput = read <$> readFile "./fixtures/input3.txt"