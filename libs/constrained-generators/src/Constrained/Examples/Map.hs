{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Constrained.Examples.Map where

import Data.Map (Map)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word
import GHC.Generics

import Constrained

mapElemSpec :: Specification BaseFn (Map Int (Bool, Int))
mapElemSpec = constrained $ \m ->
  [ assert $ m /=. lit mempty
  , forAll' (rng_ m) $ \_ b ->
      [0 <. b, b <. 10]
  ]

mapPairSpec :: Specification BaseFn (Map Int Int, Set Int)
mapPairSpec = constrained' $ \m s ->
  subset_ (dom_ m) s

mapEmptyDomainSpec :: Specification BaseFn (Map Int Int)
mapEmptyDomainSpec = constrained $ \m ->
  subset_ (dom_ m) mempty -- mempty in the Monoid instance (Term fn (Set a))

mapSubSize :: Specification BaseFn (Map Int Int)
mapSubSize = constrained $ \s ->
  2 ==. 12 - (sizeOf_ s)

knownDomainMap :: Specification BaseFn (Map Int Int)
knownDomainMap = constrained $ \m ->
  [ dom_ m ==. lit (Set.fromList [1, 2])
  , not_ $ 0 `elem_` rng_ m
  ]

data Three = One | Two | Three
  deriving (Ord, Eq, Show, Generic)

instance HasSimpleRep Three
instance BaseUniverse fn => HasSpec fn Three

mapSizeConstrained :: Specification BaseFn (Map Three Int)
mapSizeConstrained = constrained $ \m -> size_ (dom_ m) <=. 3

sumRange :: Specification BaseFn (Map Word64 Word64)
sumRange = constrained $ \m -> sum_ (rng_ m) ==. lit 10

fixedRange :: Specification BaseFn (Map Int Int)
fixedRange = constrained $ \m ->
  [ forAll (rng_ m) (\x -> x ==. 5)
  , assert $ (sizeOf_ m) ==. 1
  ]
