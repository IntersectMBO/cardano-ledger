{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Bench where

import Constrained
import Constrained.Test
import Control.DeepSeq
import Criterion
import Data.Map (Map)
import Data.Set (Set)

benchmarks :: Benchmark
benchmarks =
  bgroup
    "constrained"
    [ benchSpec 10 30 "TrueSpec@Map" (TrueSpec :: Spec BaseFn (Map Int Int))
    , benchSpec 10 30 "TrueSpec@[]" (TrueSpec :: Spec BaseFn [Int])
    , benchSpec 10 30 "TrueSpec@Set" (TrueSpec :: Spec BaseFn (Set Int))
    , benchSpec
        10
        30
        "TrueSpec@RoseTree"
        (giveHint (Nothing, 30) <> TrueSpec :: Spec RoseFn (RoseTree Int))
    , benchSpec 10 30 "roseTreeMaybe" roseTreeMaybe
    , benchSpec 10 30 "listSumPair" (listSumPair @Int)
    ]

benchSpec :: (HasSpec fn a, NFData a) => Int -> Int -> String -> Spec fn a -> Benchmark
benchSpec seed size nm spec =
  bench (unlines [nm, show (genFromSpecWithSeed seed size spec)]) $
    nf (genFromSpecWithSeed seed size) spec
