{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Babbage.Core (
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  ppProtocolVersionL,
 )
import Cardano.Ledger.BaseTypes (Inject (..), ProtVer (..), natVersion)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (
  Data (..),
  Datum (..),
  SLanguage (..),
  dataToBinaryData,
  hashPlutusScript,
 )
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Babbage.ImpTest (
  AlonzoEraImp,
  ImpInit,
  LedgerSpec,
  getsPParams,
  submitTx,
  submitTx_,
 )
import Test.Cardano.Ledger.Common (SpecWith, describe, it, when)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common (mkAddr)
import Test.Cardano.Ledger.Plutus.Examples (inputsOverlapsWithRefInputs)

spec :: forall era. (AlonzoEraImp era, BabbageEraTxBody era) => SpecWith (ImpInit (LedgerSpec era))
spec = describe "UTXO" $ do
  describe "Reference scripts" $ do
    it "Reference inputs can overlap with regular inputs in PlutusV2" $ do
      let
        txOut =
          mkBasicTxOut
            ( mkAddr
                (hashPlutusScript (inputsOverlapsWithRefInputs SPlutusV2))
                StakeRefNull
            )
            (inject $ Coin 1_000_000)
            & datumTxOutL .~ Datum (dataToBinaryData . Data $ PV1.I 0)
      tx <-
        submitTx $
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL .~ SSeq.singleton txOut
      let txIn = txInAt 0 tx
      majorVer <- pvMajor <$> getsPParams ppProtocolVersionL
      when (majorVer < natVersion @9 || majorVer > natVersion @10) $
        submitTx_ @era $
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
            & bodyTxL . referenceInputsTxBodyL .~ Set.singleton txIn
