module Utils.BenchMark where

import Control.DeepSeq (NFData, force)
import Control.Exception (handle)
import GHC.IO.Exception (ExitCode)
import Test.Tasty.Bench (bench, defaultMain, nf)

-- very simple way to benchmark pure parts of my code
-- thing is though I've not really been decoupling parsing and solving that much
-- sometimes the solution is basically just "write a complicated parser and then do something simple"
-- and my parsers usually go in the `getInput :: IO Blah` part
-- so I'd need to refactor a little to use this to get true benchmarks...
runBenchMark :: (NFData b, Show b) => (a -> b) -> a -> IO ()
runBenchMark f x = do
  let result = force (f x)

  print "here's the answer!"
  print result

  handle
    ((\_ -> pure ()) :: ExitCode -> IO ())
    (defaultMain [bench "main" $ nf f x])
