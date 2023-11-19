module Day2Part1 where

import Text.Read (readMaybe)

data Direction = Forwards | Down | Up deriving (Show)

data Command = Command Direction Int deriving (Show)

data Displacement = Disp Int Int deriving (Show)

main :: IO ()
main = do
  contents <- getLines "./fixtures/input2.txt"
  let res = foldData <$> traverse stringToCommand contents
  print res
  let prdct = takeProduct <$> res
  print prdct

takeProduct :: Displacement -> Int
takeProduct (Disp x y) = x * y

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

foldData :: [Command] -> Displacement
foldData = foldl updateDisplacement (Disp 0 0)

updateDisplacement :: Displacement -> Command -> Displacement
updateDisplacement (Disp x0 y0) (Command Forwards x) = Disp (x0 + x) y0
updateDisplacement (Disp x0 y0) (Command Down y) = Disp x0 (y0 + y)
updateDisplacement (Disp x0 y0) (Command Up y) = Disp x0 (y0 - y)

stringToCommand :: String -> Maybe Command
stringToCommand s = do
  let w = words s
  a <- nth 0 w
  b <- nth 1 w
  moveBy <- readInt b
  direction <- stringToDirection a
  return (Command direction moveBy)

stringToDirection :: String -> Maybe Direction
stringToDirection "forward" = Just Forwards
stringToDirection "down" = Just Down
stringToDirection "up" = Just Up
stringToDirection _ = Nothing

readInt :: String -> Maybe Int
readInt = readMaybe

nth :: Int -> [a] -> Maybe a
nth n xs = do
  let ys = (take (n + 1) . drop n) xs
  if null ys then Nothing else Just . head $ ys
