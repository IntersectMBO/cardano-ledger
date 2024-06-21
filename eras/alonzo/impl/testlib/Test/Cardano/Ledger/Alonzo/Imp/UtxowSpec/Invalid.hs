{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxowSpec.Invalid (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Core (
  AlonzoEraTxWits (..),
  networkIdTxBodyL,
  scriptIntegrityHashTxBodyL,
 )
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosPredFailure (..),
  AlonzoUtxowPredFailure (..),
 )
import Cardano.Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxOut (dataHashTxOutL)
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus
import Cardano.Ledger.Shelley.LedgerState (epochStatePoolParamsL, nesEsL)
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxCert
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq ((:<|)))
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Alonzo.ImpTest (AlonzoEraImp, expectPhase2Invalid, fixupPPHash)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsNoDatum, redeemerSameAsDatum)
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( AlonzoEraImp era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "Invalid transactions" $ do
  let resetAddrWits tx = updateAddrTxWits $ tx & witsTxL . addrTxWitsL .~ []
  let fixupResetAddrWits = fixupPPHash >=> resetAddrWits

  it "MissingRedeemers" $ forM_ (eraLanguages @era) $ \lang -> do
    logEntry $ "Testing for " ++ show lang
    let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
    txIn <- produceScript scriptHash
    let missingRedeemer = mkSpendingPurpose $ AsItem txIn
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    withPostFixup (fixupResetAddrWits . (witsTxL . rdmrsTxWitsL .~ Redeemers mempty)) $
      submitFailingTx
        tx
        [ injectFailure $
            MissingRedeemers [(missingRedeemer, scriptHash)]
        , injectFailure $
            CollectErrors [NoRedeemer missingRedeemer]
        ]

  it "MissingRequiredDatums" $ forM_ (eraLanguages @era) $ \lang -> do
    logEntry $ "Testing for " ++ show lang
    let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
    txIn <- produceScript scriptHash
    let tx = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    let missingDatum = hashData @era (Data (P.I 3))
    withPostFixup (fixupResetAddrWits . (witsTxL . datsTxWitsL .~ mempty)) $
      submitFailingTx
        tx
        [ injectFailure $
            MissingRequiredDatums [missingDatum] []
        ]

  it "NotAllowedSupplementalDatums" $ forM_ (eraLanguages @era) $ \lang -> do
    logEntry $ "Testing for " ++ show lang
    let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
    txIn <- produceScript scriptHash
    let extraDatumHash = hashData @era (Data (P.I 30))
    let extraDatum = Data (P.I 30)
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ [txIn]
            & witsTxL . datsTxWitsL .~ TxDats (Map.singleton extraDatumHash extraDatum)
    submitFailingTx
      tx
      [ injectFailure $
          NotAllowedSupplementalDatums [extraDatumHash] []
      ]

  it "PPViewHashesDontMatch" $ forM_ (eraLanguages @era) $ \lang -> do
    logEntry $ "Testing for " ++ show lang
    let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)
    txIn <- produceScript scriptHash
    tx <- fixupTx $ mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]

    impAnn "Mismatched " $ do
      wrongIntegrityHash <- arbitrary
      wrongIntegrityHashTx <-
        resetAddrWits $ tx & bodyTxL . scriptIntegrityHashTxBodyL .~ SJust wrongIntegrityHash
      withNoFixup $
        submitFailingTx
          wrongIntegrityHashTx
          [ injectFailure $
              PPViewHashesDontMatch
                (SJust wrongIntegrityHash)
                (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
          ]
    impAnn "Missing" $ do
      missingIntegrityHashTx <-
        resetAddrWits $ tx & bodyTxL . scriptIntegrityHashTxBodyL .~ SNothing
      withNoFixup $
        submitFailingTx
          missingIntegrityHashTx
          [ injectFailure $
              PPViewHashesDontMatch SNothing (tx ^. bodyTxL . scriptIntegrityHashTxBodyL)
          ]

  it "UnspendableUTxONoDatumHash" $ forM_ (eraLanguages @era) $ \lang -> do
    logEntry $ "Testing for " ++ show lang
    let scriptHash = withSLanguage lang (hashPlutusScript . redeemerSameAsDatum)

    txIn <- impAnn "Produce script at a txout with a missing datahash" $ do
      let addr = Addr Testnet (ScriptHashObj scriptHash) StakeRefNull
      let tx =
            mkBasicTx mkBasicTxBody
              & bodyTxL . outputsTxBodyL .~ [mkBasicTxOut addr (inject (Coin 10))]
      let resetDataHash = dataHashTxOutL .~ SNothing
      let resetTxOutDataHash =
            bodyTxL . outputsTxBodyL
              %~ ( \case
                    h :<| r -> resetDataHash h :<| r
                    _ -> error "Expected non-empty outputs"
                 )

      txInAt (0 :: Int)
        <$> withPostFixup
          (fixupResetAddrWits <$> resetTxOutDataHash)
          (submitTx tx)
    let tx = mkBasicTx (mkBasicTxBody & inputsTxBodyL .~ [txIn])
    if lang >= PlutusV3
      then -- PlutusV3 no longer requires a spending Datum, but it should still fail since the
      -- actual script expects it
        expectPhase2Invalid tx
      else submitFailingTx tx [injectFailure $ UnspendableUTxONoDatumHash [txIn]]

  it "ExtraRedeemers" $ do
    let scriptHash = withSLanguage PlutusV1 (hashPlutusScript . redeemerSameAsDatum)
    txIn <- produceScript scriptHash
    let prp = MintingPurpose (AsIx 2)
    dt <- arbitrary
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ [txIn]
            & witsTxL . rdmrsTxWitsL <>~ Redeemers (Map.singleton prp (dt, ExUnits 0 0))
    let submit = submitFailingTx tx [injectFailure $ ExtraRedeemers [prp]]
    if eraProtVerLow @era < natVersion @9
      then -- PlutusPurpose serialization was fixed in Conway
        withCborRoundTripFailures submit
      else submit

  it "No ExtraRedeemers on same script certificates" $
    forM_ ([minBound .. eraMaxLanguage @era] :: [Language]) $ \lang -> do
      Positive n <- arbitrary
      replicateM_ n registerPool
      pools <- getsNES $ nesEsL . epochStatePoolParamsL
      poolId <- elements $ Map.keys pools
      let scriptHash = withSLanguage lang (hashPlutusScript . alwaysSucceedsNoDatum)
          cred = ScriptHashObj scriptHash
          certs =
            [ RegTxCert cred
            , DelegStakeTxCert cred poolId
            , UnRegTxCert cred
            ]
      tx <- submitTx $ mkBasicTx (mkBasicTxBody & certsTxBodyL .~ certs)
      let Redeemers redeemers = tx ^. witsTxL . rdmrsTxWitsL
      Map.keys redeemers
        `shouldBe` [ CertifyingPurpose (AsIx 1)
                   , CertifyingPurpose (AsIx 2)
                   ]
  it "Wrong network ID" $ do
    submitFailingTx
      (mkBasicTx mkBasicTxBody & bodyTxL . networkIdTxBodyL .~ SJust Mainnet)
      [injectFailure $ WrongNetworkInTxBody Testnet Mainnet]

  it "Missing 2-phase script witness" $ do
    const $ pendingWith "not implemented yet"

  it "Redeemer with incorrect label" $ do
    const $ pendingWith "not implemented yet"

  it "Phase 1 script failure" $ do
    const $ pendingWith "not implemented yet"

  it "Valid transaction marked as invalid" $ do
    const $ pendingWith "not implemented yet"

  it "Invalid transaction marked as valid" $ do
    const $ pendingWith "not implemented yet"

  it "Too many execution units for tx" $ do
    const $ pendingWith "not implemented yet"

  it "Missing signature for collateral input" $ do
    const $ pendingWith "not implemented yet"

  it "Insufficient collateral" $ do
    const $ pendingWith "not implemented yet"

  it "Unacceptable extra redeemer" $ do
    const $ pendingWith "not implemented yet"

  it "Multiple equal plutus-locked certs" $ do
    const $ pendingWith "not implemented yet"

  it "No cost model" $ do
    const $ pendingWith "not implemented yet"
