{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Era (
  EraTest,
  ShelleyEraTest (..),
) where

import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.State
import Data.Default
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()

class
  ( EraTest era
  , ShelleyEraScript era
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  ) =>
  ShelleyEraTest era
  where
  -- | This is a helper function that is equivalent to `registerShelleyAccount` except it will work
  -- in all eras, since the `Ptr` argument is optional
  registerTestAccount ::
    HasCallStack =>
    Credential 'Staking ->
    Maybe Ptr ->
    CompactForm Coin ->
    Maybe (KeyHash 'StakePool) ->
    Maybe DRep ->
    Accounts era ->
    Accounts era
  default registerTestAccount ::
    (HasCallStack, ShelleyEraAccounts era) =>
    Credential 'Staking ->
    Maybe Ptr ->
    CompactForm Coin ->
    Maybe (KeyHash 'StakePool) ->
    Maybe DRep ->
    Accounts era ->
    Accounts era
  registerTestAccount cred mPtr deposit mStakePool mDRep =
    case mPtr of
      Nothing -> error "When registering Account in Shelley through Babbage eras Ptr is required"
      Just ptr ->
        case mDRep of
          Nothing -> registerShelleyAccount cred ptr deposit mStakePool
          Just _ -> error "Delegation to DRep is not supported until Conway"

instance EraTest ShelleyEra

instance ShelleyEraTest ShelleyEra
