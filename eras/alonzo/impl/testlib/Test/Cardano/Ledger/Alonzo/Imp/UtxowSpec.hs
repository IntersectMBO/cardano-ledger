{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec (spec, alonzoToConwaySpec) where

import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Shelley.Core (ShelleyEraTxCert, TopTx)
import Data.Default (Default)
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid as Invalid
import qualified Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Valid as Valid
import Test.Cardano.Ledger.Alonzo.ImpTest
import Test.Cardano.Ledger.Common

spec ::
  forall era.
  ( AlonzoEraImp era
  , Default (LevelTxInfo TopTx era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  describe "UTXOW" $ do
    Valid.spec
    Invalid.spec

alonzoToConwaySpec ::
  forall era.
  ( AlonzoEraImp era
  , ShelleyEraTxCert era
  , Default (LevelTxInfo TopTx era)
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
alonzoToConwaySpec = do
  describe "UTXOW" $ do
    describe "Certificates without deposits" $ do
      Valid.alonzoToConwaySpec
      Invalid.alonzoToConwaySpec
