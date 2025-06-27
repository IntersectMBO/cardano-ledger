{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Shelley.Era (
  EraTest (..),
  ShelleyEraTest,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Core (EraTxOut (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Transition
import Cardano.Ledger.State
import Data.Default
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Arbitrary ()
import Test.Cardano.Ledger.Shelley.TreeDiff ()
import Cardano.Ledger.Plutus (emptyCostModels)

class
  ( EraTest era
  , ShelleyEraScript era
  , EraTransition era
  , Arbitrary (TransitionConfig era)
  , Eq (StashedAVVMAddresses era)
  , Show (StashedAVVMAddresses era)
  , ToExpr (StashedAVVMAddresses era)
  , NFData (StashedAVVMAddresses era)
  , Default (StashedAVVMAddresses era)
  , Arbitrary (StashedAVVMAddresses era)
  , ToExpr (ScriptsNeeded era)
  ) =>
  ShelleyEraTest era

instance EraTest ShelleyEra where
  validTxOut scripts txOut =
    case txOut ^. addrTxOutL of
      Addr _ (KeyHashObj _) _ -> True
      Addr _ (ScriptHashObj sh) _ -> Map.member sh scripts
      AddrBootstrap {} -> False
  zeroCostModels = emptyCostModels

instance ShelleyEraTest ShelleyEra
