{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Special tracing code usefull for debugging tests
module Test.Cardano.Ledger.Babbage.Trace (trace, ptrace, occaisionally) where

import Cardano.Ledger.Pretty (PrettyA (..), ppMap)
import Data.Hashable (Hashable (..))
import Data.Map (Map)
import Debug.Trace (trace)

ptrace :: PrettyA t => [Char] -> t -> a -> a
ptrace x y z = trace ("\n" ++ show (prettyA y) ++ "\n" ++ show x) z

instance (PrettyA x, PrettyA y) => PrettyA (Map x y) where
  prettyA m = ppMap prettyA prettyA m

-- | turn on trace appromimately 1 in 'n' times it is called.
occaisionally :: Hashable a => a -> Int -> String -> String
occaisionally x n s = if mod (hash x) n == 0 then trace s s else s
