module Utils.PSQ (PSQ, empty, insert, minView, null, singleton) where

-- actually I think HashPSQ is better, let's see if this all works first though!
import qualified Data.OrdPSQ as Q (OrdPSQ, empty, insert, minView, null, singleton)
import Prelude hiding (null)

-- like an OrdPSQ but doesn't use values (i.e. only has keys and priorities)

type PSQ k p = Q.OrdPSQ k p ()

empty :: PSQ k p
empty = Q.empty

insert :: (Ord k, Ord p) => k -> p -> PSQ k p -> PSQ k p
insert key priority = Q.insert key priority ()

minView :: (Ord k, Ord p) => PSQ k p -> Maybe (k, p, PSQ k p)
minView = fmap (\(key, priority, _, psq) -> (key, priority, psq)) . Q.minView

null :: PSQ k p -> Bool
null = Q.null

singleton :: k -> p -> PSQ k p
singleton key priority = Q.singleton key priority ()
