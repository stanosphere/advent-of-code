-- I used this in an everybody codes problem a while back and have ported it here!
-- It's probably way less efficient than a real version that uses trees and such
-- But will probably do for now...
-- Based on https://en.wikipedia.org/wiki/Disjoint-set_data_structure

module Utils.UnionFind (UnionFind (UF), fromList, unionClusters, clusterMap) where

import qualified Data.Map as M
import qualified Data.Set as S

-- find the leader of a cluster of elements
-- union clusters of elements
data UnionFind a = UF
  { _leaderMap :: M.Map a a,
    _clusterMap :: M.Map a (S.Set a)
  }

clusterMap :: UnionFind a -> M.Map a (S.Set a)
clusterMap = _clusterMap

-- create singleton clusters from a list of elements
fromList :: (Ord a) => [a] -> UnionFind a
fromList xs = UF leaderMap clusterMap
  where
    leaderMap = M.fromList (zip xs xs)
    clusterMap = M.fromList (zip xs (map S.singleton xs))

inSameCluster :: (Ord a) => UnionFind a -> a -> a -> Bool
inSameCluster uf x y = clusterLeader uf x == clusterLeader uf y

clusterLeader :: (Ord a) => UnionFind a -> a -> a
clusterLeader uf x = _leaderMap uf M.! x

unionClusters :: (Ord a) => UnionFind a -> a -> a -> UnionFind a
unionClusters uf x y =
  -- should really only move stuff over in the smaller cluster tbh...
  -- but for this problem probably don't need to worry too much
  if xLeader == yLeader then uf else UF newLeaderMap newClusterMap
  where
    xLeader = clusterLeader uf x
    yLeader = clusterLeader uf y
    -- only need stuff below here if x and y are in different clusters
    clusterX = _clusterMap uf M.! xLeader
    clusterY = _clusterMap uf M.! yLeader
    unionCluster = S.union clusterX clusterY
    newClusterMap = M.delete yLeader . M.insert xLeader unionCluster . _clusterMap $ uf
    newLeaderMap = foldr (`M.insert` xLeader) (_leaderMap uf) . S.toList $ clusterY