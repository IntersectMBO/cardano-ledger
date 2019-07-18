{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Spec.Chain.STS.Properties where

import           Control.Lens ((^.), (^..))
import           Data.Foldable (traverse_)
import           Data.List.Ordered (nubSortBy)
import           Data.Ord (Down (Down), comparing)
import           Hedgehog (MonadTest, Property, assert, failure, forAll, property, withTests, (===))

import           Control.State.Transition
import           Control.State.Transition.Generator (TraceLength (Maximum), classifyTraceLength,
                     traceSigGen)
import qualified Control.State.Transition.Generator as TransitionGenerator
import           Control.State.Transition.Trace

import           Ledger.Delegation

import           Cardano.Spec.Chain.STS.Block
import           Cardano.Spec.Chain.STS.Rule.Chain
import           Ledger.Core (BlockCount (BlockCount))

slotsIncrease :: Property
slotsIncrease = property $ do
  let (maxTraceLength, step) = (1000, 100)
  tr <- forAll $ traceSigGen (Maximum maxTraceLength) (sigGenChain NoGenDelegation NoGenUTxO)
  classifyTraceLength tr maxTraceLength step
  slotsIncreaseInTrace tr

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = slots === nubSortBy (comparing Down) slots
  where blocks = traceSignals NewestFirst tr
        slots = blocks ^.. traverse . bHeader . bhSlot

blockIssuersAreDelegates :: Property
blockIssuersAreDelegates =
  withTests 200 $ property $ do
    let (maxTraceLength, step) = (1000, 100)
    tr <- forAll $ traceSigGen (Maximum maxTraceLength) (sigGenChain GenDelegation NoGenUTxO)
    classifyTraceLength tr maxTraceLength step
    checkBlockIssuersAreDelegates tr
  where
    checkBlockIssuersAreDelegates :: MonadTest m => Trace CHAIN -> m ()
    checkBlockIssuersAreDelegates tr =
       traverse_ checkIssuer $ preStatesAndSignals OldestFirst tr
       where
         checkIssuer :: MonadTest m => (State CHAIN, Signal CHAIN) -> m ()
         checkIssuer (st, bk) =
           case delegatorOf dm issuer of
             Just _ -> pure ()
             Nothing -> failure
           where
             issuer = bk ^. bHeader . bhIssuer
             dm = st ^. disL . delegationMap

onlyValidSignalsAreGenerated :: Property
onlyValidSignalsAreGenerated =
  withTests 100 $ TransitionGenerator.onlyValidSignalsAreGenerated @CHAIN 250

signersListIsBoundedByK :: Property
signersListIsBoundedByK = property $ do
  let maxTraceLength = 1000
  tr <- forAll $ traceSigGen (Maximum maxTraceLength) (sigGenChain GenDelegation NoGenUTxO)
  signersListIsBoundedByKInTrace tr
  where
    signersListIsBoundedByKInTrace :: MonadTest m => Trace CHAIN -> m ()
    signersListIsBoundedByKInTrace tr =
      traverse_ (signersListIsBoundedByKInState k) $ traceStates OldestFirst tr
      where
        (_, _, _, _, k) = _traceEnv @CHAIN tr

        signersListIsBoundedByKInState :: MonadTest m => BlockCount -> State CHAIN -> m ()
        signersListIsBoundedByKInState (BlockCount k') (_sLast, sgs, _h, _utxoSt, _ds, _us) =
          assert $ length sgs <= fromIntegral k'
