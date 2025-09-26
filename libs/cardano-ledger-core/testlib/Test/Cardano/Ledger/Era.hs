{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Era (
  EraTest (..),
  registerTestAccount,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR)
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Genesis
import Cardano.Ledger.Plutus (CostModels)
import Cardano.Ledger.State
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import Data.Typeable
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.TreeDiff ()

class
  ( -- Core
    EraTx era
  , EraTxOut era
  , EraTxBody era
  , EraTxAuxData era
  , EraTxWits era
  , EraScript era
  , EraPParams era
  , EraBlockBody era
  , EraTxCert era
  , -- State
    EraCertState era
  , EraGov era
  , EraStake era
  , EraUTxO era
  , EraAccounts era
  , EraGenesis era
  , -- Arbitrary Core
    Arbitrary (Tx era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxBody era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (TxWits era)
  , Arbitrary (Script era)
  , Arbitrary (PParamsHKD Identity era)
  , Arbitrary (PParamsHKD StrictMaybe era)
  , Arbitrary (TxCert era)
  , Arbitrary (Value era)
  , Arbitrary (CompactForm (Value era))
  , -- Arbitrary State
    Arbitrary (CertState era)
  , Arbitrary (GovState era)
  , Arbitrary (InstantStake era)
  , Arbitrary (Accounts era)
  , Arbitrary (AccountState era)
  , -- ToExpr Core
    ToExpr (Tx era)
  , ToExpr (TxOut era)
  , ToExpr (TxBody era)
  , ToExpr (TxAuxData era)
  , ToExpr (TxWits era)
  , ToExpr (Script era)
  , ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (TxCert era)
  , ToExpr (Value era)
  , ToExpr (CompactForm (Value era))
  , -- ToExpr State
    ToExpr (CertState era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  , ToExpr (Accounts era)
  , ToExpr (AccountState era)
  , -- Twiddle
    Twiddle (Value era)
  , Twiddle (CompactForm (Value era))
  , Twiddle (TxOut era)
  , Twiddle (TxCert era)
  , Twiddle (PParams era)
  , Twiddle (PParamsUpdate era)
  , Twiddle (Script era)
  , Twiddle (TxAuxData era)
  , Twiddle (TxWits era)
  , Twiddle (TxBody era)
  , Twiddle (Tx era)
  , -- Un-annotated DecCBOR instances
    DecCBOR (Script era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody era)
  , DecCBOR (Tx era)
  , -- TranslationContext
    Eq (TranslationContext era)
  , Show (TranslationContext era)
  , Typeable (TranslationContext era)
  , ToJSON (TranslationContext era)
  , FromJSON (TranslationContext era)
  , Arbitrary (TranslationContext era)
  ) =>
  EraTest era
  where
  zeroCostModels :: CostModels

  -- | This is a helper function that allows for creation of an `AccountState` in era agnostic
  -- fashion. There is no equivalent function outside of testing since arguments required for
  -- creation of `AccountState` varies between eras and we can get away with such function in
  -- testing because we allow for such function to be partial.
  mkTestAccountState ::
    HasCallStack =>
    Maybe Ptr ->
    CompactForm Coin ->
    Maybe (KeyHash 'StakePool) ->
    Maybe DRep ->
    AccountState era

  accountsFromAccountsMap :: Map.Map (Credential 'Staking) (AccountState era) -> Accounts era

-- | This is a helper function that uses `mkTestAccountState` to register an account.
registerTestAccount ::
  (HasCallStack, EraTest era) =>
  Credential 'Staking ->
  Maybe Ptr ->
  CompactForm Coin ->
  Maybe (KeyHash 'StakePool) ->
  Maybe DRep ->
  Accounts era ->
  Accounts era
registerTestAccount cred mPtr deposit mStakePool mDRep =
  addAccountState cred (mkTestAccountState mPtr deposit mStakePool mDRep)
