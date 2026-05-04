{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.Rules
import qualified Test.Cardano.Ledger.Allegra.Imp as Allegra
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as UTXO
import Test.Cardano.Ledger.Mary.ImpTest

spec ::
  ( MaryEraImp era
  , Event (EraRule "RUPD" era) ~ RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Allegra.spec era
  describe "MaryEra Onwards" $
    withImpInitEachEraVersion era $ do
      UTXO.spec
