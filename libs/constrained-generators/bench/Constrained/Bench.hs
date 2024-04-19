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
import Control.DeepSeq
import Criterion
import Data.Map (Map)
import Data.Set (Set)
import Data.Tree

type RoseFn = Fix (OneofL (TreeFn : BaseFns))

benchmarks :: Benchmark
benchmarks =
  bgroup
    "constrained"
    [ benchSpec 10 30 "TrueSpec@Map" (TrueSpec :: Specification BaseFn (Map Int Int))
    , benchSpec 10 30 "TrueSpec@[]" (TrueSpec :: Specification BaseFn [Int])
    , benchSpec 10 30 "TrueSpec@Set" (TrueSpec :: Specification BaseFn (Set Int))
    , benchSpec
        10
        30
        "TrueSpec@Tree"
        (giveHint (Nothing, 30) <> TrueSpec :: Specification RoseFn (Tree Int))
    , benchSpec 10 30 "roseTreeMaybe" roseTreeMaybe
    , benchSpec 10 30 "listSumPair" listSumPair
    ]

roseTreeMaybe :: Specification RoseFn (Tree (Maybe (Int, Int)))
roseTreeMaybe = constrained $ \t ->
  [ forAll' t $ \mp ts ->
      forAll ts $ \t' ->
        onJust mp $ \p ->
          onJust (rootLabel_ t') $ \p' ->
            fst_ p' ==. snd_ p
  , forAll' t $ \mp _ -> isJust mp
  , genHint (Nothing, 10) t
  ]

listSumPair :: Specification BaseFn [(Int, Int)]
listSumPair = constrained $ \xs ->
  [ assert $ foldMap_ fst_ xs ==. 100
  , forAll' xs $ \x y -> [20 <. x, x <. 30, y <. 100]
  ]

benchSpec :: (HasSpec fn a, NFData a) => Int -> Int -> String -> Specification fn a -> Benchmark
benchSpec seed size nm spec =
  bench (unlines [nm, show (genFromSpecWithSeed seed size spec)]) $
    nf (genFromSpecWithSeed seed size) spec
