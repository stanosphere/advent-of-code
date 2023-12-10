module Utils.Grouping (groupMap, groupMapReduce) where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map
  ( Map,
    fromList,
  )

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

-- TODO add scala groupBy here