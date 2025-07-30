{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Mary (MaryEra)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Shelley.Rules (ShelleyUtxoPredFailure, ShelleyUtxowPredFailure)
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Mary.ImpTest (EraSpecificSpec, MaryEraImp)
import Test.Cardano.Ledger.Shelley.ImpTest (LedgerSpec)

spec ::
  forall era.
  ( MaryEraImp era
  , EraSpecificSpec era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  Spec
spec = do
  AllegraImp.spec @era
  describe "MaryImpSpec" $ withImpInit @(LedgerSpec era) $ do
    Utxo.spec

instance EraSpecificSpec MaryEra
