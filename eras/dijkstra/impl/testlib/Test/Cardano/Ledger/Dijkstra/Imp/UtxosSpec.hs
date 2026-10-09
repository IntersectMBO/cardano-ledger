{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Alonzo.Scripts (eraLanguages)
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Dijkstra.Core (
  DijkstraEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
 )
import Cardano.Ledger.Plutus (Language (..), hashPlutusScript, withSLanguage)
import qualified Data.OSet.Strict as OSet
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Common (SpecWith, describe, forM_, it)
import Test.Cardano.Ledger.Dijkstra.ImpTest (
  DijkstraEraImp,
  ImpInit,
  LedgerSpec,
  submitTxAnn_,
 )
import Test.Cardano.Ledger.Plutus.Examples (purposeIsWellformedNoDatum)

spec :: forall era. DijkstraEraImp era => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXOS" $ do
  describe "Plutus" $ do
    forM_ [l | l <- eraLanguages @era, l >= PlutusV4] $ \lang ->
      withSLanguage lang $ \slang ->
        describe "purposeIsWellformedNoDatum" $ do
          it "Passes with guarding purpose" $ do
            let sh = hashPlutusScript $ purposeIsWellformedNoDatum slang
            submitTxAnn_ "Submit tx with a Plutus guard" $
              mkBasicTx mkBasicTxBody
                & bodyTxL . guardsTxBodyL .~ OSet.singleton (ScriptHashObj sh)
