{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Rules.Deleg (
  tests,
) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Shelley (hardforkAlonzoAllowMIRTransfer)
import Cardano.Ledger.Shelley.API (ShelleyDELEG)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.Rules (DelegEnv (..))
import Cardano.Ledger.Shelley.State
import Data.Foldable (fold)
import Data.Foldable as F (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Extras (view)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen (..))
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Rules.TestChain (
  delegTraceFromBlock,
  forAllChainTrace,
  traceLen,
 )
import Test.Cardano.Ledger.Shelley.Utils (
  ChainProperty,
 )
import Test.Control.State.Transition.Trace (
  SourceSignalTarget (..),
  sourceSignalTargets,
 )
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC
import Test.QuickCheck (
  Property,
  Testable (..),
  conjoin,
  counterexample,
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- | Various properties of the DELEG STS Rule, tested on longer traces
-- (double the default length)
tests ::
  forall era.
  ( EraGen era
  , EraStake era
  , ShelleyEraAccounts era
  , ChainProperty era
  , QC.HasTrace (CHAIN era) (GenEnv MockCrypto era)
  ) =>
  TestTree
tests =
  testProperty "properties of the DELEG STS" $
    forAllChainTrace @era traceLen defaultConstants $ \tr -> do
      conjoin $
        map chainProp (sourceSignalTargets tr)
  where
    delegProp :: DelegEnv era -> SourceSignalTarget (ShelleyDELEG era) -> Property
    delegProp denv delegSst =
      conjoin
        [ keyRegistration delegSst
        , keyDeRegistration delegSst
        , keyDelegation delegSst
        , balancesSumInvariant delegSst
        , checkInstantaneousRewards denv delegSst
        ]
    chainProp :: SourceSignalTarget (CHAIN era) -> Property
    chainProp (SourceSignalTarget {source = chainSt, signal = block}) =
      let delegInfo = delegTraceFromBlock chainSt block
          delegEnv = fst delegInfo
          delegTr = snd delegInfo
          delegSsts = sourceSignalTargets delegTr
       in conjoin (map (delegProp delegEnv) delegSsts)

-- | Check stake key registration
keyRegistration ::
  (EraCertState era, ShelleyEraTxCert era) =>
  SourceSignalTarget (ShelleyDELEG era) ->
  Property
keyRegistration
  SourceSignalTarget
    { signal = RegTxCert cred
    , target = targetSt
    } =
    conjoin
      [ counterexample
          "a newly registered key should have a reward account"
          (isAccountRegistered cred (targetSt ^. certDStateL . accountsL))
      , counterexample
          "a newly registered key should have a reward account with 0 balance"
          ( ((^. balanceAccountStateL) <$> lookupAccountState cred (targetSt ^. certDStateL . accountsL))
              === Just mempty
          )
      ]
keyRegistration _ = property ()

-- | Check stake key de-registration
keyDeRegistration ::
  (EraCertState era, ShelleyEraTxCert era) =>
  SourceSignalTarget (ShelleyDELEG era) ->
  Property
keyDeRegistration
  SourceSignalTarget
    { signal = UnRegTxCert cred
    , target = targetSt
    } =
    conjoin
      [ counterexample
          "a deregistered stake key should no longer be in the rewards mapping"
          (not (isAccountRegistered cred (targetSt ^. certDStateL . accountsL)))
      , counterexample
          "a deregistered stake key should no longer be in the delegations mapping"
          (isNothing (lookupStakePoolDelegation cred (targetSt ^. certDStateL . accountsL)))
      ]
keyDeRegistration _ = property ()

-- | Check stake key delegation
keyDelegation ::
  (EraCertState era, ShelleyEraTxCert era) =>
  SourceSignalTarget (ShelleyDELEG era) ->
  Property
keyDelegation
  SourceSignalTarget
    { signal = DelegStakeTxCert stakeCred poolId
    , target = targetSt
    } =
    conjoin
      [ counterexample
          "a delegated key should have a reward account"
          (isAccountRegistered stakeCred (targetSt ^. certDStateL . accountsL))
      , counterexample
          "a registered stake credential should be delegated"
          (lookupStakePoolDelegation stakeCred (targetSt ^. certDStateL . accountsL) === Just poolId)
      ]
keyDelegation _ = property ()

-- | Check that the sum of balances does not change and that each element
-- that is either removed or added has a zero balance.
balancesSumInvariant :: EraCertState era => SourceSignalTarget (ShelleyDELEG era) -> Property
balancesSumInvariant
  SourceSignalTarget {source, target} =
    let accountsBalances ds = Map.map (^. balanceAccountStateL) (ds ^. accountsL . accountsMapL)
        sourceBalances = accountsBalances (source ^. certDStateL)
        targetBalances = accountsBalances (target ^. certDStateL)
        balancesSum = F.foldl' (<>) mempty
     in conjoin
          [ counterexample
              "sum of balances should not change"
              (balancesSum sourceBalances === balancesSum targetBalances)
          , counterexample
              "dropped elements have a zero reward balance"
              (null (Map.filter (/= mempty) $ sourceBalances `Map.difference` targetBalances))
          , counterexample
              "added elements have a zero reward balance"
              (null (Map.filter (/= mempty) $ targetBalances `Map.difference` sourceBalances))
          ]

checkInstantaneousRewards ::
  (EraPParams era, EraCertState era, ShelleyEraTxCert era, AtMostEra "Babbage" era) =>
  DelegEnv era ->
  SourceSignalTarget (ShelleyDELEG era) ->
  Property
checkInstantaneousRewards
  denv
  SourceSignalTarget {source, signal, target} =
    case signal of
      MirTxCert (MIRCert ReservesMIR (StakeAddressesMIR irwd)) ->
        conjoin
          [ counterexample
              "a ReservesMIR certificate should add all entries to the `irwd` mapping"
              (Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRReserves $ dsIRewards (target ^. certDStateL)))
          , counterexample
              "a ReservesMIR certificate should add the total value to the `irwd` map, overwriting any existing entries"
              ( if hardforkAlonzoAllowMIRTransfer . view ppProtocolVersionL $ ppDE denv
                  then -- In the Alonzo era, repeated fields are added
                    fold (iRReserves $ dsIRewards (source ^. certDStateL))
                      `addDeltaCoin` fold irwd
                      == fold (iRReserves $ dsIRewards (target ^. certDStateL))
                  else -- Prior to the Alonzo era, repeated fields overridden
                    fold (iRReserves (dsIRewards (source ^. certDStateL)) Map.\\ irwd)
                      `addDeltaCoin` fold irwd
                      == fold (iRReserves $ dsIRewards (target ^. certDStateL))
              )
          ]
      MirTxCert (MIRCert TreasuryMIR (StakeAddressesMIR irwd)) ->
        conjoin
          [ counterexample
              "a TreasuryMIR certificate should add all entries to the `irwd` mapping"
              (Map.keysSet irwd `Set.isSubsetOf` Map.keysSet (iRTreasury $ dsIRewards (target ^. certDStateL)))
          , counterexample
              "a TreasuryMIR certificate should add* the total value to the `irwd` map"
              ( if hardforkAlonzoAllowMIRTransfer . view ppProtocolVersionL . ppDE $ denv
                  then -- In the Alonzo era, repeated fields are added
                    fold (iRTreasury $ dsIRewards (source ^. certDStateL))
                      `addDeltaCoin` fold irwd
                      == fold (iRTreasury $ dsIRewards (target ^. certDStateL))
                  else -- Prior to the Alonzo era, repeated fields overridden
                    fold (iRTreasury (dsIRewards (source ^. certDStateL)) Map.\\ irwd)
                      `addDeltaCoin` fold irwd
                      == fold (iRTreasury $ dsIRewards (target ^. certDStateL))
              )
          ]
      _ -> property ()
