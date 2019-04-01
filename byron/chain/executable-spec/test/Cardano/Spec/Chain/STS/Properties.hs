{-# LANGUAGE TypeFamilies #-}
module Cardano.Spec.Chain.STS.Properties where

import Control.Lens ((^..), (^.))
import Data.Foldable (traverse_)
import Data.List.Ordered (sort, nub)
import Hedgehog (MonadTest, Property, assert, failure, forAll, property, withTests)

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain
import Ledger.Delegation

slotsIncrease :: Property
slotsIncrease = property $ forAll trace >>= slotsIncreaseInTrace

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = assert $ slots == sortedSlots && nub slots == slots
  where blocks = traceSignals OldestFirst tr
        slots = blocks ^.. traverse . bHeader . bhSlot
        sortedSlots = sort slots

blockIssuersAreDelegates :: Property
blockIssuersAreDelegates =
  withTests 1000 $ property $ forAll trace >>= checkBlockIssuersAreDelegates
  where
    checkBlockIssuersAreDelegates :: MonadTest m => Trace CHAIN -> m ()
    checkBlockIssuersAreDelegates tr =
       traverse_ checkIssuer $ preStatesAndSignals OldestFirst tr
       where
         checkIssuer :: MonadTest m => (State CHAIN, Signal CHAIN) -> m ()
         checkIssuer (st, bk) =
           case delegatorOf dm issuer of
             Just _ -> pure $! ()
             Nothing -> failure
           where
             issuer = bk ^. bHeader . bhIssuer
             dm = st ^. disL . delegationMap
