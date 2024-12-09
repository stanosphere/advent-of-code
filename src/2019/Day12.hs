module Day12 where

import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

data MoonPosition = MP {_x :: Int, _y :: Int, _z :: Int} deriving (Show)

data MoonVelocity = MV {_vx :: Int, _vy :: Int, _vz :: Int} deriving (Show)

data MoonState = MS {_moonId :: Int, _position :: MoonPosition, _velocity :: MoonVelocity} deriving (Show)

type MoonsState = [MoonState]

part1 :: IO Int
part1 = sum . map energy . (!! 1000) . iterate updateState <$> getInput

energy :: MoonState -> Int
energy (MS _ (MP x y z) (MV vx vy vz)) = (sum . map abs $ [x, y, z]) * (sum . map abs $ [vx, vy, vz])

updateState :: [MoonState] -> [MoonState]
updateState ms = map (\m -> updateState' (otherMoons m) m) ms
  where
    otherMoons m = filter ((/= _moonId m) . _moonId) ms

updateState' :: [MoonState] -> MoonState -> MoonState
updateState' otherMoons moon = updateDistance . foldl (flip updateVelocity) moon $ otherMoons

-- this is less disgusting than the velocity one but may also benefit from lenses
updateDistance :: MoonState -> MoonState
updateDistance (MS moonId (MP x y z) (MV vx vy vz)) =
  MS
    moonId
    (MP (x + vx) (y + vy) (z + vz))
    (MV vx vy vz)

-- this is DISGUSTING, I think I'll refactor to use lenses
updateVelocity :: MoonState -> MoonState -> MoonState
updateVelocity otherMoon moon =
  MS
    (_moonId moon)
    (_position moon)
    ( MV
        (updateVelocityComponent (_x . _position $ otherMoon) (_x . _position $ moon) (_vx . _velocity $ moon))
        (updateVelocityComponent (_y . _position $ otherMoon) (_y . _position $ moon) (_vy . _velocity $ moon))
        (updateVelocityComponent (_z . _position $ otherMoon) (_z . _position $ moon) (_vz . _velocity $ moon))
    )

updateVelocityComponent :: Int -> Int -> Int -> Int
updateVelocityComponent otherMoonPos moonPos v
  | otherMoonPos < moonPos = v - 1
  | otherMoonPos > moonPos = v + 1
  | otherwise = v

getInput :: IO [MoonState]
getInput =
  zipWith (\i mp -> MS i mp (MV 0 0 0)) [0 ..]
    . map (unsafeParse moonParser)
    . lines
    <$> readFile "./fixtures/input12.txt"

moonParser :: Parser MoonPosition
moonParser = do
  _ <- P.string "<x="
  x <- intParser
  _ <- P.string ", y="
  y <- intParser
  _ <- P.string ", z="
  z <- intParser
  _ <- P.string ">"
  return (MP x y z)
  where
    intParser :: Parser Int
    intParser = read <$> P.many1 (P.digit P.<|> P.char '-')

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res