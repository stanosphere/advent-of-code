{-# HLINT ignore "Use <$>" #-}
module Day4 where

import Data.Foldable
import Data.Map qualified as M (Map, delete, elems, empty, filter, fromList, insert, keys, map, null, size, toList, (!))
import Data.Set qualified as S (Set, fromList, intersection, size)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Card = Card {cardId :: Int, winningNumbers :: S.Set Int, myNumbers :: S.Set Int} deriving (Show)

-- 28538, 0.03 secs
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input4.txt"
  return . sum . map (getScoreForCard . parseCard) $ inp

-- this naive approach takes 19.84 seconds
-- can definitely do better by making a sort of graph of card relationships
-- will be interesting to see how much better!
-- 9425061
part2BruteForce :: IO (Maybe Int)
part2BruteForce = do
  inp <- getLines "./fixtures/input4.txt"
  return . fmap snd . find (null . fst) . bruteForce . M.fromList . map ((\c -> (cardId c, c)) . parseCard) $ inp

-- thought process behind this is to parse the cards into a DAG
-- then I know what depends on what
-- then I can use 2018 Day 7's algo to work out the order in which I can process the cards
-- given that it's just a case of "rolling up" (i.e. each card's value is the sum of the values of the cards it depends on plus it's own value which is just one)
-- runs in 0.05 seconds so this is hundreds of times better than the brute force approach, arguably 3 orders of magnitude!
part2Quick :: IO Int
part2Quick = do
  inp <- getLines "./fixtures/input4.txt"
  let cardGraph = makeCardGraph . M.fromList . map ((\c -> (cardId c, c)) . parseCard) $ inp
  let order = workOutOrderOfRollUp cardGraph
  return . sum . M.elems . foldl (applyRollUpStep cardGraph) M.empty $ order

-- part 1 stuff
getScoreForCard :: Card -> Int
getScoreForCard c = if winCount > 0 then 2 ^ (winCount - 1) else 0
  where
    winCount = getWinCount c

getWinCount :: Card -> Int
getWinCount (Card _ winningNumbers nyNumbers) = S.size (S.intersection winningNumbers nyNumbers)

-- so let's brute force this to begin with...
-- I reckon we can sort of recurse through our stack of cards:
-- remove cards from the stack one by one and add the stuff they add to the stack and the result stack
-- actually the result stack can just be a counter...
bruteForce :: M.Map Int Card -> [([Card], Int)]
bruteForce cardMap = iterate step (M.elems cardMap, M.size cardMap)
  where
    step :: ([Card], Int) -> ([Card], Int)
    step ([], wins) = ([], wins)
    step (h : t, wins) = let newCards = getNewCards h in (newCards ++ t, wins + length newCards)
    getNewCards :: Card -> [Card]
    getNewCards c = map (cardMap M.!) . take (getWinCount c) $ [(1 + cardId c) ..]

-- quick part 2 below here
applyRollUpStep :: TaskMap Int -> M.Map Int Int -> Int -> M.Map Int Int
applyRollUpStep lookupMap resultMap key =
  let rolledUpValue = sum . map (resultMap M.!) . (lookupMap M.!) $ key
   in M.insert key (rolledUpValue + 1) resultMap

makeCardGraph :: M.Map Int Card -> TaskMap Int
makeCardGraph cardMap = M.map getNewCards cardMap
  where
    getNewCards :: Card -> [Int]
    getNewCards c = (map (cardId . (cardMap M.!)) . take (getWinCount c)) $ [(1 + cardId c) ..]

type TaskMap a = M.Map a [a]

data State a = State {taskMap :: TaskMap a, stepOrder :: [a]} deriving (Show)

workOutOrderOfRollUp :: Ord a => TaskMap a -> [a]
workOutOrderOfRollUp tm = unsafeGet . fmap (reverse . stepOrder) . find (M.null . taskMap) . iterate step $ State tm []
  where
    step :: Ord a => State a -> State a
    step (State taskMap stepOrder) = State taskMap' stepOrder'
      where
        nextTaskToDo = minimum . M.keys . M.filter null $ taskMap
        taskMap' = M.map (filter (/= nextTaskToDo)) . M.delete nextTaskToDo $ taskMap
        stepOrder' = nextTaskToDo : stepOrder

unsafeGet :: Maybe a -> a
unsafeGet (Just x) = x
unsafeGet Nothing = error "oh dear tried to get something from nothing!"

-- parsing stuff below here
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseCard :: String -> Card
parseCard = unsafeParse cardParser

cardParser :: Parser Card
cardParser = do
  P.string "Card" *> P.spaces
  cardId <- intParser
  P.char ':' *> P.spaces
  winners <- numbersParser
  P.char '|' *> P.spaces
  myNumbers <- numbersParser
  return (Card cardId winners myNumbers)
  where
    numbersParser = S.fromList <$> P.endBy intParser P.spaces
    intParser = read <$> P.many1 P.digit

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res
