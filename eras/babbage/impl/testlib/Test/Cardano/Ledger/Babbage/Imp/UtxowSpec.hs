{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec, babbageEraSpecificSpec) where

import Cardano.Ledger.Babbage.Core (ShelleyEraTxCert)
import Test.Cardano.Ledger.Alonzo.ImpTest (ImpInit, LedgerSpec)
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. BabbageEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec

babbageEraSpecificSpec ::
  forall era.
  ( BabbageEraImp era
  , ShelleyEraTxCert era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  describe "UTXOW - certificates without deposits" $ do
    Valid.babbageEraSpecificSpec
