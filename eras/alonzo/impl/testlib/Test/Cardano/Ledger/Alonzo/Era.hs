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
import Cardano.Ledger.Alonzo.Scripts ()
import Cardano.Ledger.Alonzo.UTxO
import Data.TreeDiff
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.TreeDiff ()
import Test.Cardano.Ledger.Mary.Era

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
  ) =>
  AlonzoEraTest era

instance EraTest AlonzoEra

instance ShelleyEraTest AlonzoEra

instance AllegraEraTest AlonzoEra

instance MaryEraTest AlonzoEra

instance AlonzoEraTest AlonzoEra
