{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.TxInfo where

import Cardano.Ledger.Alonzo.TxInfo (ExtendedUTxO (..))
import qualified Cardano.Ledger.Babbage.TxBody as B
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Plutus.Language (Language (..))
import Data.Proxy (Proxy (..))
import Test.Tasty (TestTree)

import qualified Test.Cardano.Ledger.Babbage.TxInfo as B

txInfoTests ::
  forall era.
  (ExtendedUTxO era, EraTx era, B.BabbageEraTxBody era, B.BabbageTxInfoTests era) =>
  Proxy era ->
  TestTree
txInfoTests p = B.txInfoTestsV2 p PlutusV3 -- The V2 tests in Babbage should all hold for V3
