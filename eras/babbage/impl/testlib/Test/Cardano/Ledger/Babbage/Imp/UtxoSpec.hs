{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Babbage.Core (
  BabbageEraTxBody (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  KeyRole (..),
 )
import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus (SLanguage (..), hashPlutusScript)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import Test.Cardano.Ledger.Babbage.ImpTest (
  AlonzoEraImp,
  ImpInit,
  LedgerSpec,
  submitTx,
  submitTx_,
 )
import Test.Cardano.Ledger.Common (SpecWith, describe, it)
import Test.Cardano.Ledger.Imp.Common (mkAddr)
import Test.Cardano.Ledger.Plutus.Examples (inputsIsSubsetOfRefInputs)
import Test.Cardano.Ledger.Core.Utils (txInAt)

spec :: forall era. (AlonzoEraImp era, BabbageEraTxBody era) => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "Reference scripts" $ do
    it "Reference inputs can overlap with regular inputs in PlutusV2" $ do
      let
        txOut =
          mkBasicTxOut
            ( mkAddr
                (ScriptHashObj @'Payment $ hashPlutusScript (inputsIsSubsetOfRefInputs SPlutusV2))
                StakeRefNull
            )
            (inject $ Coin 1_000_000)
      tx <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL .~ SSeq.singleton txOut
      let txIn = txInAt (0 :: Integer) tx
      submitTx_ @era $
        mkBasicTx mkBasicTxBody
          & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
          & bodyTxL . referenceInputsTxBodyL .~ Set.singleton txIn
