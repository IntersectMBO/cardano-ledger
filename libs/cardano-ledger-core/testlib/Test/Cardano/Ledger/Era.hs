{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Test.Cardano.Ledger.Era (
  EraTest (..),
  registerTestAccount,
  accountsFromUMap,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.State
import Cardano.Ledger.UMap
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Functor.Identity
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable
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
  , EraSegWits era
  , EraTxCert era
  , -- State
    EraCertState era
  , EraGov era
  , EraStake era
  , EraUTxO era
  , EraAccounts era
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
  , -- ToExpr State
    ToExpr (CertState era)
  , ToExpr (GovState era)
  , ToExpr (InstantStake era)
  , ToExpr (Accounts era)
  , ToExpr (AccountState era)
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

  accountsToUMap :: Accounts era -> UMap

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

-- This is a termporary converter, which is used to test some functionality until UMap is completely removed
accountsFromUMap :: EraTest era => UMap -> Accounts era
accountsFromUMap umap =
  Map.foldrWithKey' register def (umElems umap)
  where
    register cred (UMElem mRD ptrSet smStakePool smDRep) =
      case mRD of
        SNothing -> error "Invalid UMap state: missing RDPair"
        SJust RDPair {rdReward, rdDeposit} ->
          let mPtr =
                case Set.toList ptrSet of
                  [] -> Nothing
                  [ptr] -> Just ptr
                  ptrs -> error $ "Invalid UMap state: Can't have more than one pointer: " <> show ptrs
              mStakePool = strictMaybeToMaybe smStakePool
              mDRep = strictMaybeToMaybe smDRep
           in addToBalanceAccounts (Map.singleton cred rdReward)
                . registerTestAccount cred mPtr rdDeposit mStakePool mDRep
