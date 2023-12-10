module Utils.Grouping (groupMap, groupMapReduce, groupBy') where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map, fromList)

-- inspired by scala function of the same name
groupMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupMap keyBy mapBy =
  fromList
    . map (\xs -> (keyBy . head $ xs, map mapBy xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy

-- inspired by the scala function of the same name
groupMapReduce :: Ord k => (a -> k) -> (a -> v) -> (v -> v -> v) -> [a] -> Map k v
groupMapReduce keyBy mapBy combine =
  fromList
    . map (\xs -> (keyBy . head $ xs, foldl1 combine . map mapBy $ xs))
    . groupBy ((==) `on` keyBy)
    . sortOn keyBy

-- this works like scala's groupBy in the sense that elements need not be adjacent in the original list to be grouped
groupBy' :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy' f =
  fromList
    . map (\xs -> (f . head $ xs, xs))
    . groupBy ((==) `on` f)
    . sortOn f