{-# LANGUAGE TypeFamilies #-}
module Cardano.Spec.Chain.STS.Properties where

import Control.Lens ((^..), (^.))
import Data.Foldable (traverse_)
import Data.List.Ordered (nubSortBy)
import Data.Ord (Down(Down), comparing)
import Hedgehog
  (MonadTest, Property, (===), failure, forAll, property, withTests)

import Control.State.Transition
import Control.State.Transition.Generator
import Control.State.Transition.Trace

import Ledger.Delegation

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.Chain

slotsIncrease :: Property
slotsIncrease = property $ do
  tr <- forAll $ traceSigGen 200 (sigGenChain NoGenDelegation NoGenUTxO)
  classifyTraceLength tr 200 50
  slotsIncreaseInTrace tr

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = slots === nubSortBy (comparing Down) slots
  where blocks = traceSignals NewestFirst tr
        slots = blocks ^.. traverse . bHeader . bhSlot

blockIssuersAreDelegates :: Property
blockIssuersAreDelegates =
  withTests 200 $ property $ do
    tr <- forAll $ traceSigGen 200 (sigGenChain GenDelegation NoGenUTxO)
    classifyTraceLength tr 200 50
    checkBlockIssuersAreDelegates tr
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
