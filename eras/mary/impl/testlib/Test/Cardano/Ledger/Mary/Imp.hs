{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Mary.Imp (spec, Allegra.shelleyToBabbageSpec) where

import Cardano.Ledger.Core
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Control.State.Transition (Event)
import qualified Test.Cardano.Ledger.Allegra.Imp as Allegra
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as UTXO
import Test.Cardano.Ledger.Mary.ImpTest

spec ::
  ( MaryEraImp era
  , Event (EraRule "RUPD" era) ~ Shelley.RupdEvent
  ) =>
  proxy era ->
  Spec
spec era = do
  Allegra.spec era
  describe "MaryEra Onwards" $
    withImpInitEachEraVersion era $ do
      UTXO.spec
