{-# LANGUAGE TupleSections #-}

module Day5 where

import Control.Monad ((>=>))
import Data.Hash.MD5 (Str (Str), md5s)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

md5 :: String -> String
md5 = md5s . Str

-- takes well over a minute (83.39 secs)
-- must be a better way!!!!
part1 :: IO [Char]
part1 = do
  input <- getInput
  return
    . map (!! 5)
    . take 8
    . filter ("00000" `isPrefixOf`)
    . map (\n -> md5 (input ++ show n))
    $ [1 ..]

-- takes well over two minutes (131.82 secs)
-- must be a better way!!!!
-- plus I manually made the password from the output of this
part2 = do
  input <- getInput
  return
    . sortOn (fst . snd)
    . zip [0 ..]
    . take 10
    . mapMaybe (\x -> (,x !! 6) <$> readInt [x !! 5])
    . filter ("00000" `isPrefixOf`)
    . map (\n -> md5 (input ++ show n))
    $ [1 ..]

readInt :: String -> Maybe Int
readInt = readMaybe >=> (\i -> if i >= 8 then Nothing else Just i)

getInput :: IO String
getInput = readFile "./fixtures/input5.txt"