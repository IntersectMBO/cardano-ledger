{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Mary.Imp (spec) where

import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Shelley.Rules (
  ShelleyPoolPredFailure,
  ShelleyUtxoPredFailure,
  ShelleyUtxowPredFailure,
 )
import qualified Test.Cardano.Ledger.Allegra.Imp as AllegraImp
import Test.Cardano.Ledger.Imp.Common
import qualified Test.Cardano.Ledger.Mary.Imp.UtxoSpec as Utxo
import Test.Cardano.Ledger.Mary.ImpTest

spec ::
  forall era.
  ( MaryEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxoPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" ShelleyPoolPredFailure era
  ) =>
  Spec
spec = do
  AllegraImp.spec @era
  describe "MaryImpSpec" $
    withEachEraVersion @era $
      Utxo.spec
