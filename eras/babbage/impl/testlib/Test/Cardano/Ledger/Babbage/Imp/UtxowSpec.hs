{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec) where

import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp)
import Test.Cardano.Ledger.Imp.Common

spec :: forall era. BabbageEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec
