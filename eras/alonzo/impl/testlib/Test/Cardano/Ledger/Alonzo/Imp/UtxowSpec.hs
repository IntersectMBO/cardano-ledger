{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec (spec, alonzoToConwaySpec) where

import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Common

spec :: forall era. AlonzoEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec

alonzoToConwaySpec ::
  forall era. (AlonzoEraImp era, ShelleyEraTxCert era) => SpecWith (ImpInit (LedgerSpec era))
alonzoToConwaySpec = do
  describe "UTXOW" $ do
    describe "Certificates without deposits" $ do
      Valid.alonzoToConwaySpec
      Invalid.alonzoToConwaySpec
