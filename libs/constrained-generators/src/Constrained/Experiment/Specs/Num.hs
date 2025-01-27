{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | All the HasSpec instances for a few types with Num instances
module Constrained.Experiment.Specs.Num ((+.), negate_, (>=.), (>.), (-.)) where

import Constrained.Experiment.Base
import Constrained.Experiment.NumSpec
import Constrained.Experiment.TheKnot
import Data.Ratio
import Data.Word
import GHC.Int
import GHC.Natural

-- =================================================
infix 4 +.
(+.) :: NumLike a => Term a -> Term a -> Term a
(+.) = addFn

negate_ :: NumLike a => Term a -> Term a
negate_ = negateFn

infix 4 -.
(-.) :: Numeric n => Term n -> Term n -> Term n
(-.) x y = addFn x (negateFn y)

instance HasSpec Int where
  type TypeSpec Int = NumSpec Int
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec (Ratio Integer) where
  type TypeSpec (Ratio Integer) = NumSpec (Ratio Integer)
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Natural where
  type TypeSpec Natural = NumSpec Natural
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec (NumSpecInterval (Just lo) (Just hi)) =
    if hi >= lo
      then MemberSpec (pure (fromIntegral @Natural @Integer (hi - lo + 1)))
      else MemberSpec (pure 0)
  cardinalTypeSpec (NumSpecInterval Nothing (Just hi)) =
    MemberSpec (pure (fromIntegral @Natural @Integer hi + 1))
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word8 where
  type TypeSpec Word8 = NumSpec Word8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  typeSpecOpt = notInNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word16 where
  type TypeSpec Word16 = NumSpec Word16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word32 where
  type TypeSpec Word32 = NumSpec Word32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Word64 where
  type TypeSpec Word64 = NumSpec Word64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int8 where
  type TypeSpec Int8 = NumSpec Int8
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int16 where
  type TypeSpec Int16 = NumSpec Int16
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int32 where
  type TypeSpec Int32 = NumSpec Int32
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Int64 where
  type TypeSpec Int64 = NumSpec Int64
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec = cardinalNumSpec
  guardTypeSpec = guardNumSpec

instance HasSpec Float where
  type TypeSpec Float = NumSpec Float
  emptySpec = emptyNumSpec
  combineSpec = combineNumSpec
  genFromTypeSpec = genFromNumSpec
  shrinkWithTypeSpec = shrinkWithNumSpec
  conformsTo = conformsToNumSpec
  toPreds = toPredsNumSpec
  cardinalTypeSpec _ = TrueSpec
  guardTypeSpec = guardNumSpec
