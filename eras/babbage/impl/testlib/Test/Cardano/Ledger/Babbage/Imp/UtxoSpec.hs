{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.UtxoSpec (spec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.BaseTypes (Inject (..), ProtVer (..), StrictMaybe (..), natVersion)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Credential (StakeReference (..))
import Cardano.Ledger.Plutus (
  Language (..),
  SLanguage (..),
  hashPlutusScript,
  mkInlineDatum,
  withSLanguage,
 )
import qualified Data.ByteString as BS
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as PV1
import Test.Cardano.Ledger.Babbage.ImpTest (
  AlonzoEraImp,
  ImpInit,
  LedgerSpec,
  freshKeyAddr_,
  getsPParams,
  sendCoinTo,
  submitFailingTx,
  submitTx,
  submitTx_,
 )
import Test.Cardano.Ledger.Common (SpecWith, describe, it, when)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common (mkAddr)
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum, inputsOverlapsWithRefInputs)

spec ::
  forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  ) =>
  SpecWith (ImpInit (LedgerSpec era))
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
            & datumTxOutL .~ mkInlineDatum (PV1.I 0)
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

  it "Incorrect collateral total" $ do
    let scriptHash = withSLanguage PlutusV2 (hashPlutusScript . alwaysSucceedsWithDatum)
        txOut =
          mkBasicTxOut (mkAddr scriptHash StakeRefNull) mempty
            & datumTxOutL .~ mkInlineDatum (PV1.I 1)
        tx1 = mkBasicTx $ mkBasicTxBody & outputsTxBodyL .~ [txOut]
    txIn <- txInAt 0 <$> submitTx tx1
    addr <- freshKeyAddr_
    coll <- sendCoinTo addr $ Coin 5_000_000
    let collReturn = mkBasicTxOut addr . inject $ Coin 2_000_000
        tx2 =
          mkBasicTx $
            mkBasicTxBody
              & inputsTxBodyL .~ [txIn]
              & collateralInputsTxBodyL .~ [coll]
              & collateralReturnTxBodyL .~ SJust collReturn
              & totalCollateralTxBodyL .~ SJust (Coin 1_000_000)
    submitFailingTx
      tx2
      [injectFailure (IncorrectTotalCollateralField (DeltaCoin 3_000_000) (Coin 1_000_000))]

  -- TxOut too large for the included ADA, using a large inline datum
  it "Min-utxo value with output too large" $ do
    pp <- getsPParams id
    addr <- freshKeyAddr_
    let
      amount = inject $ Coin 5_000_000
      largeDatum = PV1.B $ BS.replicate 1500 0
      txOut = mkBasicTxOut addr amount & datumTxOutL .~ mkInlineDatum largeDatum
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . outputsTxBodyL .~ [txOut])
      [ injectFailure $
          BabbageOutputTooSmallUTxO [(txOut, getMinCoinTxOut pp txOut)]
      ]
