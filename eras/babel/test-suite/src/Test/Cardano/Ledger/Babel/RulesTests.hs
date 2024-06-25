{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Babel.RulesTests (
  chainExamples,
  testTickF,
)
where

import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (ShelleyTICK, ShelleyTICKF)
import Cardano.Ledger.Shelley.LedgerState (
  EpochState (..),
  LedgerState (..),
  NewEpochState (..),
  UTxOState (..),
  totalObligation,
  utxosGovStateL,
 )
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardUpdate (..))
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Protocol.TPraos.API (GetLedgerView (..))
import Control.State.Transition.Extended (TRC (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Babel.Examples (testCHAINExample)
import Test.Cardano.Ledger.Babel.Examples.Prototype
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (Property, discard, testProperty, (===))

chainExamples :: TestTree
chainExamples =
  testGroup
    "CHAINexamples"
    [ testCase "empty block" $ testCHAINExample exEmptyBlock
    ]

-- | The reward aggregation bug described in the Shelley ledger spec in
-- section 17.4 (in the Errata) resulted in needing to use 'aggregatedRewards' to change
-- the behavior of how rewards are collected starting at protocol version 3.
-- Instead of collecting a `Coin` for each stake credential, we collect 'Set Reward'.
-- In major protocol version 2, it is impossible for this set to be empty, but sadly this
-- property is not enforced in the types. For this reason, the property test
-- 'propTickfPerservesLedgerView' removes these empty sets from an otherwise arbitrary
-- 'NewEpochState'.
filterEmptyRewards :: NewEpochState Shelley -> NewEpochState Shelley
filterEmptyRewards (NewEpochState el bprev bcur es ru pd stash) =
  NewEpochState el bprev bcur es ru' pd stash
  where
    removeEmptyRewards = Map.filter $ not . Set.null
    ru' = case ru of
      SNothing -> SNothing
      SJust (Pulsing _ _) -> SNothing
      SJust (Complete rewardUpdate) ->
        SJust . Complete $ rewardUpdate {rs = removeEmptyRewards (rs rewardUpdate)}

setDepositsToObligation :: NewEpochState Shelley -> NewEpochState Shelley
setDepositsToObligation nes = nes {nesEs = es {esLState = ls {lsUTxOState = utxoState}}}
  where
    es = nesEs nes
    ls = esLState es
    utxoState =
      (lsUTxOState ls)
        { utxosDeposited =
            totalObligation
              (lsCertState ls)
              (utxoState ^. utxosGovStateL)
        }

-- | This property test checks the correctness of the TICKF transation.
-- TICKF is used by the consensus layer to get a ledger view in a computationally
-- cheaper way than using the TICK rule.
-- Therefore TICKF and TICK need to compute the same ledger view.
propTickfPerservesLedgerView :: NewEpochState Shelley -> Property
propTickfPerservesLedgerView nes =
  let (EpochNo e) = nesEL nes
      slot = slotFromEpoch (EpochNo $ e + 1)
      nes' = setDepositsToObligation (filterEmptyRewards nes)
      tickNes = runShelleyBase $ applySTSTest @(ShelleyTICK Shelley) (TRC ((), nes', slot))
      tickFNes = runShelleyBase $ applySTSTest @(ShelleyTICKF Shelley) (TRC ((), nes', slot))
   in fromMaybe discard $ do
        Right tickNes' <- pure tickNes
        Right tickFNes' <- pure tickFNes
        pure $ currentLedgerView tickNes' === currentLedgerView tickFNes'

testTickF :: TestTree
testTickF = testProperty "TICKF properties" propTickfPerservesLedgerView
