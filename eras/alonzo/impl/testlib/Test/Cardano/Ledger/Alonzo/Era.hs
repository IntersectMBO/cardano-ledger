{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Era (
  module Test.Cardano.Ledger.Mary.Era,
  AlonzoEraTest,
) where

import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Plutus.Context
import Cardano.Ledger.Alonzo.UTxO
import Cardano.Ledger.Plutus (Language (..))
import Data.TreeDiff
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.Binary.Annotator ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Common (Arbitrary)
import Test.Cardano.Ledger.Mary.Era
import Test.Cardano.Ledger.Plutus (zeroTestingCostModels)

class
  ( MaryEraTest era
  , EraPlutusContext era
  , AlonzoEraTx era
  , AlonzoEraTxAuxData era
  , AlonzoEraUTxO era
  , ToExpr (PlutusScript era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsIxItem era)
  , Script era ~ AlonzoScript era
  , EraPlutusTxInfo PlutusV1 era
  , Arbitrary (PlutusPurpose AsIx era)
  ) =>
  AlonzoEraTest era

instance EraTest AlonzoEra where
  zeroCostModels = zeroTestingCostModels [PlutusV1]

  mkTestAccountState = mkShelleyTestAccountState

  accountsFromAccountsMap = shelleyAccountsFromAccountsMap

instance ShelleyEraTest AlonzoEra

instance AllegraEraTest AlonzoEra

instance MaryEraTest AlonzoEra

instance AlonzoEraTest AlonzoEra
