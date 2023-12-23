{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use <$>" #-}

module Day19Part2 where

import Data.List.Extra (splitOn)
import Data.Map qualified as M (Map, adjust, elems, fromList, (!))
import Data.Maybe (maybeToList)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

data Interval = Interval {start :: Int, end :: Int} deriving (Show)

type Part = M.Map Char Interval

data Destination = Accept | Reject | ToWorkFlow String deriving (Show, Eq)

data Rule = Rule {condition :: Condition, destination :: Destination} deriving (Show)

-- in scala I think I'd use NonEmptyList for the rules
data WorkFlow = WF {wfId :: String, rules :: [Rule]} deriving (Show)

data Op = GreaterThan | LessThan deriving (Show)

data Condition = Cond {partType :: Char, op :: Op, comparator :: Int} | Default deriving (Show)

-- oh dear
-- so part 2 is very similar to that really scary interval one from day 5
-- maybe I can use similar logic...
-- brute force definitely won't work: we'd need 256 trillion iterations
-- -- and each iteration itself could potentially be quite long: could potentially go through many workflows
-- -- and my rule of thumb is usually a billion is too many, so this is like _way_ too many
-- I think something along the lines of you start with a type called Part'
-- -- it's identical to Part except it uses intervals
-- -- and then it's a case of following the rules and splitting it up into intervals
-- -- so let's say a rule is "a < 300" you'd like make another Part' which has interval like 0 -> 299 and send it to the relevant WF
-- -- and the rest (i.e. 300 -> 4000)
-- I think this is different enough that I'll just do it in a new file (i.e. this file)
-- after much drawing I eventually came up with this rather pattern-matchy answer

-- I suppose I could implement part1 using `solve` by passing in the parts in my interval structure
-- rather than creating the seed part within the function

-- 0.04 secs
-- 124831893423809
part2 :: IO Int
part2 = do
  rawInput <- getLines "./fixtures/input19.txt"
  let rawWorkflows = head . splitOn [""] $ rawInput
  let workflows = M.fromList . map ((\wf -> (wfId wf, wf)) . unsafeParse workFlowParser) $ rawWorkflows
  let res = solve workflows
  return res

-- start in workflow named in
solve :: M.Map String WorkFlow -> Int
solve wfs = fst (applyWorkFlows' wfs (0, [(ToWorkFlow "in", initialPart)]))
  where
    initialInterval = Interval 1 4000
    initialPart = M.fromList . map (\c -> (c, initialInterval)) $ "xmas"

-- recursive boi
applyWorkFlows' :: M.Map String WorkFlow -> (Int, [(Destination, Part)]) -> (Int, [(Destination, Part)])
applyWorkFlows' _ (count, []) = (count, [])
applyWorkFlows' mp (count, partsToProcess) = (count', partsToProcess')
  where
    res = map (applyWorkFlowToPartV2 mp) partsToProcess
    count' = (+ count) . sum . map fst $ res
    partsToProcess' = concatMap snd res

-- basically just a wrapper on the "normal" version of this function
applyWorkFlowToPartV2 :: M.Map String WorkFlow -> (Destination, Part) -> (Int, [(Destination, Part)])
applyWorkFlowToPartV2 mp (ToWorkFlow wId, p) = applyWorkFlowToPart (mp M.! wId) p
applyWorkFlowToPartV2 _ _ = undefined

applyWorkFlowToPart :: WorkFlow -> Part -> (Int, [(Destination, Part)])
applyWorkFlowToPart wf p = (acceptCount, furtherWorkflows)
  where
    res = applyRulesToPart (rules wf) p
    acceptCount = sum . map (sumRatingNumbers . snd) . filter ((== Accept) . fst) $ res
    furtherWorkflows = filter (isWorkflowDest . fst) res

isWorkflowDest :: Destination -> Bool
isWorkflowDest (ToWorkFlow _) = True
isWorkflowDest _ = False

sumRatingNumbers :: Part -> Int
sumRatingNumbers = product . map getSize . M.elems

getSize :: Interval -> Int
getSize (Interval lo hi) = hi - lo + 1

-- could probably do as a fold but I find recursion slightly easier for this particular problem
applyRulesToPart :: [Rule] -> Part -> [(Destination, Part)]
applyRulesToPart [] _ = undefined -- shouldn't happen
applyRulesToPart [rule] part = maybeToList (fst (applyRuleToPart rule part))
applyRulesToPart (h : t) part = case applyRuleToPart h part of
  (Just (dest, p1), Just p2) -> (dest, p1) : applyRulesToPart t p2
  (Nothing, Just p2) -> applyRulesToPart t p2
  (Just (dest, p1), Nothing) -> [(dest, p1)]
  (Nothing, Nothing) -> []

applyRuleToPart :: Rule -> Part -> (Maybe (Destination, Part), Maybe Part)
applyRuleToPart (Rule Default dest) p = (Just (dest, p), Nothing)
applyRuleToPart (Rule (Cond pType GreaterThan comp) dest) p
  | isOnLeftOfInterval partInterval comp = (Just (dest, p), Nothing)
  | isOnRightOfInterval partInterval comp = (Nothing, Just p)
  | otherwise = (Just (dest, passing), Just failing)
  where
    partInterval = p M.! pType
    passing = M.adjust (\(Interval _ hi) -> Interval (comp + 1) hi) pType p
    failing = M.adjust (\(Interval lo _) -> Interval lo comp) pType p
applyRuleToPart (Rule (Cond pType LessThan comp) dest) p
  | isOnLeftOfInterval partInterval comp = (Nothing, Just p)
  | isOnRightOfInterval partInterval comp = (Just (dest, p), Nothing)
  | otherwise = (Just (dest, passing), Just failing)
  where
    partInterval = p M.! pType
    passing = M.adjust (\(Interval lo _) -> Interval lo (comp - 1)) pType p
    failing = M.adjust (\(Interval _ hi) -> Interval comp hi) pType p

isOnLeftOfInterval :: Interval -> Int -> Bool
isOnLeftOfInterval (Interval lo _) i = i <= lo

isOnRightOfInterval :: Interval -> Int -> Bool
isOnRightOfInterval (Interval _ hi) i = i >= hi

-- lots of parsing for this one!
unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

workFlowParser :: Parser WorkFlow
workFlowParser = do
  wId <- P.many1 P.letter
  _ <- P.char '{'
  rules <- P.sepBy ruleParser (P.char ',')
  _ <- P.char '}'
  return (WF wId rules)

ruleParser :: Parser Rule
ruleParser = P.try conditionedRuleParser <|> defaultRuleParser

defaultRuleParser :: Parser Rule
defaultRuleParser = Rule Default <$> destinationParser

conditionedRuleParser :: Parser Rule
conditionedRuleParser = do
  cond <- conditionParser
  _ <- P.char ':'
  dest <- destinationParser
  return (Rule cond dest)

conditionParser :: Parser Condition
conditionParser = do
  partType <- P.oneOf "xmas"
  op <- opParser
  comparator <- intParser
  return (Cond partType op comparator)

opParser :: Parser Op
opParser = (GreaterThan <$ P.char '>') <|> (LessThan <$ P.char '<')

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

destinationParser :: Parser Destination
destinationParser =
  (Accept <$ P.char 'A')
    <|> (Reject <$ P.char 'R')
    <|> (ToWorkFlow <$> P.many1 P.letter)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)