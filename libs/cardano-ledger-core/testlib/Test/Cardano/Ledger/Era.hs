{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Era (
  EraTest (..),
  EraSpec (..),
  ledgerEraTestMain,
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
import GHC.TypeLits (Symbol, symbolVal)
import Test.Cardano.Ledger.Binary.Golden (cborAnnGoldenSpec)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.Binary.RoundTrip (roundTripEraSpec)
import Test.Cardano.Ledger.Era.Rules
import Test.Cardano.Ledger.TreeDiff ()

class
  ( -- Core
    EraBlockBody era
  , -- State
    EraCertState era
  , EraGov era
  , EraStake era
  , EraUTxO era
  , EraAccounts era
  , EraGenesis era
  , -- Arbitrary Core
    Arbitrary (Tx TopTx era)
  , Arbitrary (TxBody TopTx era)
  , Arbitrary (TxWits era)
  , Arbitrary (TxOut era)
  , Arbitrary (TxAuxData era)
  , Arbitrary (NativeScript era)
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
    ToExpr (Tx TopTx era)
  , ToExpr (TxBody TopTx era)
  , ToExpr (TxWits era)
  , ToExpr (TxOut era)
  , ToExpr (TxAuxData era)
  , ToExpr (NativeScript era)
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
  , -- Un-annotated DecCBOR instances
    DecCBOR (Script era)
  , DecCBOR (NativeScript era)
  , DecCBOR (TxAuxData era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxBody TopTx era)
  , DecCBOR (Tx TopTx era)
  , -- TranslationContext
    Eq (TranslationContext era)
  , Show (TranslationContext era)
  , Typeable (TranslationContext era)
  , ToJSON (TranslationContext era)
  , FromJSON (TranslationContext era)
  , Arbitrary (TranslationContext era)
  , UnliftRules era (EraRulesWithFailures era)
  ) =>
  EraTest era
  where
  -- | All Ledger rules with predicate failures
  type EraRulesWithFailures era :: [Symbol]

  zeroCostModels :: CostModels

  -- | This is a helper function that allows for creation of an `AccountState` in era agnostic
  -- fashion. There is no equivalent function outside of testing since arguments required for
  -- creation of `AccountState` varies between eras and we can get away with such function in
  -- testing because we allow for such function to be partial.
  mkTestAccountState ::
    HasCallStack =>
    Maybe Ptr ->
    CompactForm Coin ->
    Maybe (KeyHash StakePool) ->
    Maybe DRep ->
    AccountState era

  accountsFromAccountsMap :: Map.Map (Credential Staking) (AccountState era) -> Accounts era

  -- | Get the full path for the era directory.
  -- An use case for this is for saving golden files in a golden test directory
  -- for each era.
  mkEraFullPath :: FilePath -> IO FilePath

  -- | Example transaction that needs to be provided for each era. Doesn't need
  -- to be valid, but all possible fields must be set to some example value.
  exampleTx :: Tx TopTx era

  -- | Example PParams used for testing. All possible fields must be set.
  examplePParams :: PParams era

  -- | Example PParamsUpdate used for testing. All possible fields must be set.
  examplePParamsUpdate :: PParamsUpdate era

class EraTest era => EraSpec era where
  -- | All of Imp spec that is applicable to this era
  eraImpSpec :: Proxy era -> Spec

-- | This is the main entry point for every era's test suite. It contains all tests that must be
-- supplied by each era through `EraSpec` type class and then some through the extra argument
ledgerEraTestMain ::
  forall era.
  EraSpec era =>
  -- | Tests that are specific to this era, if any.
  Spec ->
  IO ()
ledgerEraTestMain extraEraSpec =
  ledgerTestMain $
    describe (eraName @era) $ do
      describe "Imp" $ eraImpSpec (Proxy @era)
      describe "Binary" $ do
        describe "RoundTrip" $ do
          describe "Predicate Failures" $
            let
              go :: EraRuleProof era rs' -> Spec
              go EraRuleProofEmpty = pure ()
              go (EraRuleProofHead px@(Proxy :: Proxy r) nextRule) = do
                describe (symbolVal px) $ roundTripEraSpec @era @(EraRuleFailure r era)
                go nextRule
             in
              go $ unliftEraRuleProofs @era @(EraRulesWithFailures era)
        describe "Golden" $
          cborAnnGoldenSpec
            (mkEraFullPath @era)
            "golden/tx.cbor"
            (eraProtVerLow @era)
            (exampleTx @era)
      describe "JSON" $ do
        prop (show $ typeRep $ Proxy @(NativeScript era)) $
          roundTripAesonProperty @(NativeScript era)
        prop (show $ typeRep $ Proxy @(Script era)) $
          roundTripAesonProperty @(Script era)
        prop (show $ typeRep $ Proxy @(TxAuxData era)) $
          roundTripAesonProperty @(TxAuxData era)
        prop (show $ typeRep $ Proxy @(TxWits era)) $
          roundTripAesonProperty @(TxWits era)
      describe "Era-specific spec" extraEraSpec

-- | This is a helper function that uses `mkTestAccountState` to register an account.
registerTestAccount ::
  (HasCallStack, EraTest era) =>
  Credential Staking ->
  Maybe Ptr ->
  CompactForm Coin ->
  Maybe (KeyHash StakePool) ->
  Maybe DRep ->
  Accounts era ->
  Accounts era
registerTestAccount cred mPtr deposit mStakePool mDRep =
  addAccountState cred (mkTestAccountState mPtr deposit mStakePool mDRep)
