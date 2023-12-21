{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Day19 where

import Data.Foldable ()
import Data.List.Extra (splitOn)
import Data.Map qualified as M (Map, elems, fromList, (!))
import Data.Maybe (mapMaybe)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

type Part = M.Map Char Int

data Destination = Accept | Reject | ToWorkFlow String deriving (Show, Eq)

data Rule = Rule {condition :: Condition, destination :: Destination} deriving (Show)

data WorkFlow = WF {wfId :: String, rules :: [Rule]} deriving (Show)

data Op = GreaterThan | LessThan deriving (Show)

data Condition = Cond {partType :: Char, op :: Op, comparator :: Int} | Default deriving (Show)

part1 = do
  rawInput <- getLines "./fixtures/input19.txt"
  let [rawWorkflows, rawParts] = splitOn [""] rawInput
  let workflows = M.fromList . map ((\wf -> (wfId wf, wf)) . unsafeParse workFlowParser) $ rawWorkflows
  let parts = map (unsafeParse partParser) rawParts
  let res = sum . map sumRatingNumbers . filter ((== Accept) . applyWorkFlows workflows) $ parts

  print res

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
-- I think this is different enough that I'll just do it in a new file

sumRatingNumbers :: Part -> Int
sumRatingNumbers = sum . M.elems

-- start in workflow named in
applyWorkFlows :: M.Map String WorkFlow -> Part -> Destination
applyWorkFlows wfs p = applyWorkFlows' (ToWorkFlow "in")
  where
    applyWorkFlows' :: Destination -> Destination
    applyWorkFlows' (ToWorkFlow d) = applyWorkFlows' (applyWorkFlow p (wfs M.! d))
    applyWorkFlows' dest = dest

applyWorkFlow :: Part -> WorkFlow -> Destination
applyWorkFlow part (WF _ rules) = head . mapMaybe applyRule $ rules
  where
    applyRule (Rule condition destination) =
      if conditionToFunction condition part
        then Just destination
        else Nothing

conditionToFunction :: Condition -> (Part -> Bool)
conditionToFunction Default _ = True
conditionToFunction (Cond partType GreaterThan comparator) p = (p M.! partType) > comparator
conditionToFunction (Cond partType LessThan comparator) p = (p M.! partType) < comparator

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

-- {x=2036,m=264,a=79,s=2244}
partParser :: Parser Part
partParser = M.fromList <$> (P.char '{' *> P.sepBy tupleParser (P.char ',') <* P.char '}')

tupleParser :: Parser (Char, Int)
tupleParser = do
  partId <- P.oneOf "xmas"
  _ <- P.char '='
  partRating <- intParser
  return (partId, partRating)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)