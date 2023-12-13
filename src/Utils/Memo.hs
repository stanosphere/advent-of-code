-- taken from this gist here
-- https://gist.github.com/beala/d871ae8397167e7035f218a25ddf87dd

module Utils.Memo where

import Control.Monad.State.Strict (State, gets, modify)
import Data.Map.Strict as Map (Map, insert, lookup)

-- This is the standard fibonacci implementation with exponential complexity.
naiveFibs :: Int -> Integer
naiveFibs 0 = 0
naiveFibs 1 = 1
naiveFibs n = naiveFibs (n - 2) + naiveFibs (n - 1)

-- For reference, calculating the 35th fibonacci takes 9.49 sec.
-- λ: :set +s
-- λ: naiveFibs 35
-- 9227465
-- (9.49 secs, 4,806,880,896 bytes)

-- This can be sped up with memoization. There are several ways to do it. If
-- we're thinking imperatively, our first thought might be a mutable cache:
-- Before each recursive call, first consult the cache to see if the value has
-- already been calculated. If so, use the cached value, otherwise calculate
-- and update the cache. Mutation of the cache could be modeled inside the
-- State monad (or IO/ST).

-- Below is an attempt at expressing memoization inside the State monad. It
-- could be cleaned up and factored out a bit, but ultimately it's imperative
-- code.
stateMemoFibs :: Int -> State (Map.Map Int Integer) Integer
stateMemoFibs 0 = return 0
stateMemoFibs 1 = return 1
stateMemoFibs n = do
  -- Try and get the n-2 and n-1 fib from the cache. If they're not there,
  -- calculate them recursively and update the cache.
  n2 <- getOrUpdate (n - 2) (stateMemoFibs (n - 2))
  n1 <- getOrUpdate (n - 1) (stateMemoFibs (n - 1))
  return (n2 + n1)

-- Ask for a value in the cache. If it's not there, run the state computation
-- and insert the result into the cache.
getOrUpdate :: (Ord k) => k -> State (Map.Map k v) v -> State (Map.Map k v) v
getOrUpdate k ifEmptyState = do
  maybeVal <- gets (Map.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      ifEmpty <- ifEmptyState
      modify (Map.insert k ifEmpty)
      return ifEmpty

-- Despite the ugliness, it does work.
-- λ: evalState (stateMemoFibs  35) Map.empty
-- 9227465
-- (0.00 secs, 1,577,160 bytes)

-- Now for the nice solution. Rather than trying to model imperative code
-- inside State, we can take advantage of laziness: Calculate an infinite list
-- of fibonacci values. When a value in the list is forced, its result is
-- automatically "cached" by the runtime. If the value at that index is asked
-- for again, it needn't be re-calculated.

-- We implement this as follows: first a fibs function is mapped over a lazy
-- list from 0 to infinity. We now have a list where the nth value is the nth
-- fib. Because the list is lazy, and nothing is forcing the elements, no fibs
-- are computed yet. The memoized fibs function is simply that list with the
-- list indexing function (!!) partially applied:
lazyMemoFibs :: Int -> Integer
lazyMemoFibs = (fmap fibs [0 ..] !!)
  where
    fibs 0 = 0
    fibs 1 = 1
    fibs n = lazyMemoFibs (n - 2) + lazyMemoFibs (n - 1)

-- And it's fast.
-- λ: lazyMemoFibs 35
-- 9227465
-- (0.00 secs, 1,066,016 bytes)

-- Refs:
-- 1. The nice lazy solution was taken from: https://wiki.haskell.org/Memoization
-- 2. Tikhon Jelvis takes this idea much further in his article "Lazy Dynamic Programming": http://jelv.is/blog/Lazy-Dynamic-Programming/