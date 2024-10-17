{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo (
  genUtxoExecContext,
) where

import Cardano.Ledger.BaseTypes (ProtVer (..), natVersion)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Core (EraPParams (..), ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import qualified Lib as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen)
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  SpecTranslate (..),
  computationResultToEither,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (
  IsConwayUniv,
  UtxoExecContext (..),
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Conway.ImpTest (showConwayTxBalance)
import Test.Cardano.Ledger.Generic.GenState (
  GenEnv (..),
  GenSize (..),
  GenState (..),
  initialLedgerState,
  runGenRS,
 )
import qualified Test.Cardano.Ledger.Generic.GenState as GenSize
import qualified Test.Cardano.Ledger.Generic.PrettyCore as PP
import qualified Test.Cardano.Ledger.Generic.Proof as Proof
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)

genUtxoExecContext :: Gen (UtxoExecContext Conway)
genUtxoExecContext = do
  let proof = Proof.reify @Conway
  ueSlot <- arbitrary
  let
    genSize =
      GenSize.small
        { invalidScriptFreq = 0 -- TODO make the test work with invalid scripts
        , regCertFreq = 0
        , delegCertFreq = 0
        }
  ((uecUTxO, uecTx), gs) <-
    runGenRS proof genSize $
      genAlonzoTx proof ueSlot
  let
    txSize = uecTx ^. sizeTxF
    lState = initialLedgerState gs
    ueCertState = lsCertState lState
    uePParams =
      gePParams (gsGenEnv gs)
        & ppMaxTxSizeL .~ fromIntegral txSize
        & ppProtocolVersionL .~ ProtVer (natVersion @10) 0
    uecUtxoEnv = UtxoEnv {..}
  pure UtxoExecContext {..}

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  type ExecContext fn "UTXO" Conway = UtxoExecContext Conway

  genExecContext = genUtxoExecContext

  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec ctx _ _ = utxoTxSpec ctx

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxoStep env st sig

  extraInfo ctx env@UtxoEnv {..} st@UTxOState {..} sig =
    "Impl:\n"
      <> PP.ppString (showConwayTxBalance uePParams ueCertState utxosUtxo sig)
      <> "\n\nSpec:\n"
      <> PP.ppString
        ( either show T.unpack . runSpecTransM ctx $
            Agda.utxoDebug
              <$> toSpecRep env
              <*> toSpecRep st
              <*> toSpecRep sig
        )
