{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Era (
  module Test.Cardano.Ledger.Babbage.Era,
  ConwayEraTest,
  mkConwayTestAccountState,
) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..), EraPlutusTxInfo)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Conway.TxInfo (ConwayContextError)
import Cardano.Ledger.Plutus (Language (..))
import Data.Coerce
import Lens.Micro
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.Binary.Annotator ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( BabbageEraTest era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  , ConwayEraGov era
  , ConwayEraAccounts era
  , EraPlutusTxInfo PlutusV3 era
  , Inject (ConwayContextError era) (ContextError era)
  ) =>
  ConwayEraTest era

instance EraTest ConwayEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV3]

  mkTestAccountState _mPtr = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

instance ShelleyEraTest ConwayEra

instance AllegraEraTest ConwayEra

instance MaryEraTest ConwayEra

instance AlonzoEraTest ConwayEra

instance BabbageEraTest ConwayEra

instance ConwayEraTest ConwayEra

-- | similar to mkShelleyTestAccountState, but it ignores the mPtr, and doesn't
--   need to test that mDRep is SNothing, since this is the Conway Era, where DReps can be allocated.
mkConwayTestAccountState ::
  ConwayEraAccounts era =>
  CompactForm Coin ->
  Maybe (KeyHash 'StakePool) ->
  Maybe DRep ->
  AccountState era
mkConwayTestAccountState deposit mStakePool mDRep =
  mkConwayAccountState deposit
    & stakePoolDelegationAccountStateL .~ mStakePool
    & dRepDelegationAccountStateL .~ mDRep
