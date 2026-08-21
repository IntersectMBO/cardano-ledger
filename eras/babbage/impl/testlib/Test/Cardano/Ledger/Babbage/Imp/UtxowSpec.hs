{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Core (TopTx)
import Data.Default (Default)
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Babbage.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Babbage.ImpTest (BabbageEraImp, ImpInit, LedgerSpec)
import Test.Cardano.Ledger.Imp.Common

spec ::
  forall era.
  ( BabbageEraImp era
  , Default (LevelTxInfo TopTx era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec
