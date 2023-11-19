module Day2Part2 where

import Text.Read (readMaybe)

data Direction = Forwards | Down | Up deriving (Show)

data Command = Command Direction Int deriving (Show)

data SubmarineState = SubState {xDisplacement :: Int, yDisplacement :: Int, aim :: Int} deriving (Show)

main :: IO ()
main = do
  contents <- getLines "./fixtures/input2.txt"
  let res = foldData <$> traverse stringToCommand contents
  print res
  let prdct = takeProduct <$> res
  print prdct

takeProduct :: SubmarineState -> Int
takeProduct (SubState xDisplacement yDisplacement aim) = xDisplacement * yDisplacement

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

foldData :: [Command] -> SubmarineState
foldData = foldl updateSubState (SubState 0 0 0)

updateSubState :: SubmarineState -> Command -> SubmarineState
updateSubState (SubState x0 y0 aim0) (Command Forwards z) = SubState (x0 + z) (y0 + aim0 * z) aim0
updateSubState (SubState x0 y0 aim0) (Command Down aim) = SubState x0 y0 (aim0 + aim)
updateSubState (SubState x0 y0 aim0) (Command Up aim) = SubState x0 y0 (aim0 - aim)

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
