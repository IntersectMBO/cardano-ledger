{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Babbage.Tx (ScriptIntegrity (..), getLanguageView)
import Cardano.Ledger.BaseTypes (
  Inject (..),
  Mismatch (..),
  Network (..),
  StrictMaybe (..),
  TxIx (..),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  AlonzoEraTxBody (..),
  AlonzoEraTxWits (..),
  CoinPerByte (..),
  EraIndependentScriptIntegrity,
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  InjectRuleFailure (..),
  SafeHash,
  SafeToHash (..),
  ppCoinsPerUTxOByteL,
  txIdTx,
 )
import Cardano.Ledger.Conway.Rules (ConwayUtxowPredFailure (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference)
import Cardano.Ledger.Plutus (Language (..), SLanguage (..), hashPlutusScript)
import Cardano.Ledger.TxIn (TxIn (..))
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Conway.ImpTest
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus.Examples (alwaysSucceedsWithDatum)

spec ::
  forall era.
  ConwayEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
spec = do
  it "Fails with PPViewHashesDontMatch before PV 11" . whenMajorVersionAtMost @10 $ do
    fixedTx <- fixupTx =<< setupBadPPViewHashTx
    badScriptIntegrityHash <- arbitrary
    tx <- substituteIntegrityHashAndFixWits badScriptIntegrityHash fixedTx
    scriptIntegrityHash <- computeScriptIntegrityHash tx
    impAnn "Submit a transaction with an invalid script integrity hash"
      . withNoFixup
      $ submitFailingTx
        tx
        [ injectFailure . PPViewHashesDontMatch $
            Mismatch
              { mismatchSupplied = badScriptIntegrityHash
              , mismatchExpected = scriptIntegrityHash
              }
        ]
  it "Fails with PPViewHashesDontMatchInformative after PV 11" . whenMajorVersionAtLeast @11 $ do
    fixedTx <- fixupTx =<< setupBadPPViewHashTx
    pp <- getsPParams id
    badScriptIntegrityHash <- arbitrary
    let
      langView = [getLanguageView pp PlutusV2]
      scriptIntegrity = ScriptIntegrity @era redeemers dats langView
      redeemers = fixedTx ^. witsTxL . rdmrsTxWitsL
      dats = fixedTx ^. witsTxL . datsTxWitsL
    tx <- substituteIntegrityHashAndFixWits badScriptIntegrityHash fixedTx
    scriptIntegrityHash <- computeScriptIntegrityHash tx
    let
      mismatch =
        Mismatch
          { mismatchSupplied = badScriptIntegrityHash
          , mismatchExpected = scriptIntegrityHash
          }
    impAnn "Submit a transaction with an invalid script integrity hash"
      . withNoFixup
      $ submitFailingTx
        tx
        [ injectFailure $ ScriptIntegrityHashMismatch mismatch (SJust $ originalBytes scriptIntegrity)
        ]

setupBadPPViewHashTx ::
  forall era.
  ConwayEraImp era =>
  ImpTestM era (Tx era)
setupBadPPViewHashTx = do
  modifyPParams $ ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1)
  someKeyHash <- arbitrary @StakeReference
  let scriptTxOut =
        mkBasicTxOut
          ( Addr
              Testnet
              (ScriptHashObj (hashPlutusScript $ alwaysSucceedsWithDatum SPlutusV2))
              someKeyHash
          )
          (inject $ Coin 1_000_000)
  scriptTxIn <-
    impAnn "Submit a transaction that has a script output"
      . submitTx
      $ mkBasicTx mkBasicTxBody
        & bodyTxL . outputsTxBodyL .~ [scriptTxOut]
  pure $
    mkBasicTx mkBasicTxBody
      & bodyTxL . inputsTxBodyL .~ [TxIn (txIdTx scriptTxIn) (TxIx 0)]

substituteIntegrityHashAndFixWits ::
  forall era.
  ConwayEraImp era =>
  StrictMaybe (SafeHash EraIndependentScriptIntegrity) ->
  Tx era ->
  ImpTestM era (Tx era)
substituteIntegrityHashAndFixWits hash tx =
  let txWithNewHash =
        tx
          & bodyTxL . scriptIntegrityHashTxBodyL .~ hash
          & witsTxL .~ mkBasicTxWits
   in fixupScriptWits txWithNewHash
        >>= fixupDatums
        >>= fixupRedeemers
        >>= updateAddrTxWits
