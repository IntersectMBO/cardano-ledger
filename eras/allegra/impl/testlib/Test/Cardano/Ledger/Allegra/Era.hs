{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.Era (
  module Test.Cardano.Ledger.Shelley.Era,
  AllegraEraTest,
  allegraValidTxOut,
) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts
import Cardano.Ledger.Credential (Credential (..))
import Data.Map.Strict (Map)
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Allegra.Arbitrary ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Era
import Test.Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Plutus (emptyCostModels)

class
  ( ShelleyEraTest era
  , AllegraEraTxBody era
  , AllegraEraTxAuxData era
  , AllegraEraScript era
  ) =>
  AllegraEraTest era

allegraValidTxOut :: EraTxOut era => Map ScriptHash (Script era) -> TxOut era -> Bool
allegraValidTxOut _ txOut = case txOut ^. addrTxOutL of
  Addr _ KeyHashObj {} _ -> True
  _ -> False

instance EraTest AllegraEra where
  validTxOut = allegraValidTxOut
  zeroCostModels = emptyCostModels

instance ShelleyEraTest AllegraEra

instance AllegraEraTest AllegraEra
