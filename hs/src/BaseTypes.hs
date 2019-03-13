{-# LANGUAGE EmptyDataDecls #-}

module BaseTypes
  ( FixedPoint
  , fpPrecision
  , fpEpsilon
  ) where

import qualified Data.Fixed as FP

data E34

instance FP.HasResolution E34 where
  resolution _ = (10::Integer)^(34::Integer)

type Digits34 = FP.Fixed E34

type FixedPoint = Digits34

fpPrecision :: FixedPoint
fpPrecision = (10::FixedPoint)^(34::Integer)

fpEpsilon :: FixedPoint
fpEpsilon = (10::FixedPoint)^(17::Integer)
