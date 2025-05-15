{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Dijkstra.ImpTest () where

import Cardano.Ledger.Dijkstra (DijkstraEra)
import Test.Cardano.Ledger.Conway.ImpTest
import Cardano.Ledger.Conway.Core (TranslationContext)
import Cardano.Ledger.Plutus (SLanguage(..))

type instance TranslationContext DijkstraEra = ()

instance ShelleyEraImp DijkstraEra where
  initGenesis = pure ()

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  modifyPParams = conwayModifyPParams

  fixupTx = babbageFixupTx
  expectTxSuccess = impBabbageExpectTxSuccess

instance MaryEraImp DijkstraEra

instance AlonzoEraImp DijkstraEra where
  scriptTestContexts =
    plutusTestScripts SPlutusV1
      <> plutusTestScripts SPlutusV2
      <> plutusTestScripts SPlutusV3

instance ConwayEraImp DijkstraEra
