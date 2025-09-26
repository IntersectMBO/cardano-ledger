{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Era (
  module Test.Cardano.Ledger.Era,
  ShelleyEraTest,
  mkShelleyTestAccountState,
  nativeAlwaysFails,
  nativeAlwaysSucceeds,
  shelleyAccountsFromAccountsMap,
) where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Plutus (emptyCostModels)
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.API (ApplyTx)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Transition
import Data.Default
import qualified Data.Map.Strict as Map
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.Binary.Annotator ()
import Test.Cardano.Ledger.Shelley.Binary.Twiddle ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()

class
  ( EraTest era
  , ApplyTx era
  , ShelleyEraScript era
  , EraTransition era
  , Arbitrary (TransitionConfig era)
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , EncCBOR (StashedAVVMAddresses era)
  , DecCBOR (StashedAVVMAddresses era)
  , ToExpr (ScriptsNeeded era)
  , SafeToHash (TxWits era)
  ) =>
  ShelleyEraTest era

instance EraTest ShelleyEra where
  zeroCostModels = emptyCostModels

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

instance ShelleyEraTest ShelleyEra

mkShelleyTestAccountState ::
  (HasCallStack, ShelleyEraAccounts era) =>
  Maybe Ptr ->
  CompactForm Coin ->
  Maybe (KeyHash 'StakePool) ->
  Maybe DRep ->
  AccountState era
mkShelleyTestAccountState mPtr deposit mStakePool mDRep =
  case mPtr of
    Nothing -> error "When registering Account in Shelley through Babbage eras Ptr is required"
    Just ptr ->
      case mDRep of
        Nothing -> mkShelleyAccountState ptr deposit & stakePoolDelegationAccountStateL .~ mStakePool
        Just _ -> error "Delegation to DRep is not supported until Conway"

shelleyAccountsFromAccountsMap ::
  ( Accounts era ~ ShelleyAccounts era
  , AccountState era ~ ShelleyAccountState era
  , ShelleyEraAccounts era
  ) =>
  Map.Map (Credential 'Staking) (AccountState era) -> Accounts era
shelleyAccountsFromAccountsMap accountsMap =
  ShelleyAccounts
    { saStates = accountsMap
    , saPtrs =
        Map.fromList
          [(accountState ^. ptrAccountStateG, cred) | (cred, accountState) <- Map.toList accountsMap]
    }

-- Minimal native scripts that can be used in all post-Shelley eras

nativeAlwaysFails :: forall era. ShelleyEraScript era => Script era
nativeAlwaysFails = fromNativeScript $ RequireAnyOf []

nativeAlwaysSucceeds :: forall era. ShelleyEraScript era => Script era
nativeAlwaysSucceeds = fromNativeScript $ RequireAllOf []
