{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conway.Era (
  module Test.Cardano.Ledger.Babbage.Era,
  ConwayEraTest,
  mkConwayTestAccountState,
  conwayAccountsToUMap,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.State
import Cardano.Ledger.Plutus (Language (..))
import Cardano.Ledger.UMap
import Data.Coerce
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Paths_cardano_ledger_conway
import Test.Cardano.Ledger.Babbage.Era
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( BabbageEraTest era
  , ConwayEraTxBody era
  , ConwayEraCertState era
  , ConwayEraGov era
  , ConwayEraAccounts era
  ) =>
  ConwayEraTest era

instance EraTest ConwayEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1 .. PlutusV3]

  getEraDataFileName = getDataFileName

  mkTestAccountState _mPtr = mkConwayTestAccountState

  accountsFromAccountsMap = coerce

  accountsToUMap = conwayAccountsToUMap

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

conwayAccountsToUMap :: ConwayEraAccounts era => Accounts era -> UMap
conwayAccountsToUMap accounts =
  UMap
    { umElems = Map.map toUMElem (accounts ^. accountsMapL)
    , umPtrs = Map.empty
    }
  where
    toUMElem accountState =
      UMElem
        (SJust (RDPair (accountState ^. balanceAccountStateL) (accountState ^. depositAccountStateL)))
        Set.empty
        (maybeToStrictMaybe (accountState ^. stakePoolDelegationAccountStateL))
        (maybeToStrictMaybe (accountState ^. dRepDelegationAccountStateL))
