{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Byron.Spec.Chain.STS.Properties where

import Byron.Spec.Chain.STS.Block
import Byron.Spec.Chain.STS.Rule.Chain
import Byron.Spec.Chain.STS.Rule.Epoch (sEpoch)
import Byron.Spec.Ledger.Core (BlockCount (BlockCount), Epoch, Slot (unSlot))
import Byron.Spec.Ledger.Delegation
import Byron.Spec.Ledger.GlobalParams (slotsPerEpoch)
import Control.Arrow ((***))
import Control.State.Transition
import Control.State.Transition.Generator
  ( TraceLength (Desired, Maximum),
    classifyTraceLength,
    traceSigGen,
  )
import qualified Control.State.Transition.Generator as Transition.Generator
import Control.State.Transition.Trace
import Data.Foldable (traverse_)
import Data.List.Ordered (nubSortBy)
import Data.Ord (Down (Down), comparing)
import Hedgehog
  ( MonadTest,
    Property,
    assert,
    cover,
    failure,
    forAll,
    property,
    withTests,
    (===),
  )
import Lens.Micro ((&), (^.), (^..), _1, _5)
import Lens.Micro.Extras (view)

slotsIncrease :: Property
slotsIncrease = property $ do
  let (maxTraceLength, step) = (100, 10)
  tr <-
    forAll $
      traceSigGen
        ()
        (Maximum maxTraceLength)
        (sigGenChain NoGenDelegation NoGenUTxO NoGenUpdate)
  classifyTraceLength tr maxTraceLength step
  slotsIncreaseInTrace tr

slotsIncreaseInTrace :: MonadTest m => Trace CHAIN -> m ()
slotsIncreaseInTrace tr = slots === nubSortBy (comparing Down) slots
  where
    blocks = traceSignals NewestFirst tr
    slots = blocks ^.. traverse . bHeader . bhSlot

blockIssuersAreDelegates :: Property
blockIssuersAreDelegates =
  withTests 300 $
    property $ do
      let (maxTraceLength, step) = (100, 10)
      tr <-
        forAll $
          traceSigGen
            ()
            (Maximum maxTraceLength)
            (sigGenChain GenDelegation NoGenUTxO GenUpdate)
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
  withTests 200 $ Transition.Generator.onlyValidSignalsAreGenerated @CHAIN () 100

signersListIsBoundedByK :: Property
signersListIsBoundedByK = withTests 300 $
  property $ do
    let maxTraceLength = 100
    tr <-
      forAll $
        traceSigGen
          ()
          (Maximum maxTraceLength)
          (sigGenChain GenDelegation NoGenUTxO GenUpdate)
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

relevantCasesAreCovered :: Property
relevantCasesAreCovered = withTests 200 $
  property $ do
    tr <- forAll $ traceSigGen () (Desired 200) (sigGenChain GenDelegation NoGenUTxO NoGenUpdate)
    let certs = traceDCerts tr

    -- for at least 1% of traces...
    cover
      1
      "there are more certificates than blocks"
      (traceLength tr <= length certs)

    -- for at least 10% of traces...
    cover
      10
      "at most 75% of blocks have no certificates"
      (emptyDelegationPayloadRatio (traceDCertsByBlock tr) <= 0.75)

    -- for at least 25% of traces...
    cover
      25
      "at least 25% of delegates will delegate to this epoch"
      (0.25 <= thisEpochDelegationsRatio (epochDelegationEpoch tr))

    -- for at least 60% of traces...
    cover
      60
      "at least 50% of delegations will delegate to the next epoch"
      (0.5 <= nextEpochDelegationsRatio (epochDelegationEpoch tr))

    -- for at least 10% of traces...
    cover
      10
      "at most 30% of certificates will self-delegate"
      (selfDelegationsRatio certs <= 0.30)

    -- for at least 60% of traces...
    cover
      60
      "at least 25% delegates have multiple delegators"
      (0.25 <= multipleDelegationsRatio certs)

    -- for at least 20% of traces...
    cover
      20
      "some delegates have at least 5 corresponding delegators"
      (5 <= maxDelegationsTo certs)

    -- for at least 5% of traces...
    cover
      5
      "at most 50% of delegators change their delegation"
      (changedDelegationsRatio certs <= 0.5)

    -- for at least 20% of traces...
    cover
      20
      "some delegators have changed their delegation 5 or more times"
      (5 <= maxChangedDelegations certs)

    -- for at least 2% of traces...
    cover
      2
      "at most 25% of delegations are repeats"
      (repeatedDelegationsRatio certs <= 0.25)

    -- for at least 30% of traces...
    cover
      30
      "some delegations are repeated 10 or more times"
      (10 <= maxRepeatedDelegations certs)

    -- for at least 15% of traces...
    cover
      15
      "some blocks have 5 or more certificates"
      (5 <= maxCertsPerBlock (traceDCertsByBlock tr))

    -- for at least 50% of traces...
    cover
      50
      "there is at least one change of epoch in the trace"
      (2 <= epochBoundariesInTrace tr)

    -- for at least 30% of traces...
    cover
      30
      "there are at least 5 epoch changes in the trace"
      (5 <= epochBoundariesInTrace tr)
  where
    -- Get the epoch in which the delegation certificates of the trace were
    -- applied, paired with the epoch of the delegation certificate.
    epochDelegationEpoch :: Trace CHAIN -> [(Epoch, Epoch)]
    epochDelegationEpoch tr =
      preStatesAndSignals @CHAIN OldestFirst tr
        & fmap (sEpoch_ . view _1 *** (fmap depoch . (_bDCerts . _bBody)))
        & fmap (\(e, es) -> zip (repeat e) es)
        & concat
      where
        blockCount = _traceEnv tr ^. _5
        sEpoch_ = flip sEpoch blockCount

    -- Count the number of epoch boundaries in the trace
    epochBoundariesInTrace :: Trace CHAIN -> Int
    epochBoundariesInTrace tr =
      length $
        filter (== 0) (isAtBoundary <$> slots)
      where
        blocks = traceSignals NewestFirst tr
        slots = blocks ^.. traverse . bHeader . bhSlot
        k = _traceEnv tr ^. _5
        isAtBoundary = (`rem` slotsPerEpoch k) . unSlot

-- | Extract the delegation certificates in the blocks, in the order they would
-- have been applied.
traceDCertsByBlock :: Trace CHAIN -> [[DCert]]
traceDCertsByBlock tr = _bDCerts . _bBody <$> traceSignals OldestFirst tr

-- | Flattended list of DCerts for the given Trace
traceDCerts :: Trace CHAIN -> [DCert]
traceDCerts = concat . traceDCertsByBlock

invalidSignalsAreGenerated :: Property
invalidSignalsAreGenerated =
  withTests 100 $
    Transition.Generator.invalidSignalsAreGenerated
      @CHAIN
      ()
      [(1, invalidProofsBlockGen)]
      50
      (coverInvalidBlockProofs 20)
