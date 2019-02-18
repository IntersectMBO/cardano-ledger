{-# LANGUAGE TypeFamilies #-}
module Cardano.Spec.Chain.STS.Properties where

import Control.Lens ((^..))
import Data.List.Ordered (sort, nub)
import Hedgehog
  ( MonadTest
  , Property
  , assert
  , forAll
  , property
  )

import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

slotsIncrease :: Property
slotsIncrease = property $ forAll trace >>= slotsIncreaseInTrace

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = assert $ slots == sortedSlots && nub slots == slots
  where blocks = traceSignals OldestFirst tr
        slots = blocks ^.. traverse . bHeader . bSlot
        sortedSlots = sort slots
