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
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Core (EraPParams (..), ppMaxTxSizeL, sizeTxF)
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..), UTxOState (..))
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import qualified Data.Text as T
import Lens.Micro ((&), (.~), (^.))
import qualified Lib as Agda
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Common (Arbitrary (..), Gen, showExpr)
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  runSpecTransM,
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (
  UtxoExecContext (..),
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Conway.ImpTest (showConwayTxBalance)
import Test.Cardano.Ledger.Generic.Functions (TotalAda (..))
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

genUtxoExecContext :: Gen (UtxoExecContext ConwayEra)
genUtxoExecContext = do
  let proof = Proof.reify @ConwayEra
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

instance ExecSpecRule "UTXO" ConwayEra where
  type ExecContext "UTXO" ConwayEra = UtxoExecContext ConwayEra

  genExecContext = genUtxoExecContext

  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec ctx _ _ = utxoTxSpec ctx

  runAgdaRule env st sig =
    unComputationResult $
      Agda.utxoStep externalFunctions env st sig

  extraInfo _ ctx env@UtxoEnv {..} st@UTxOState {..} sig st' =
    PP.vcat
      [ "Impl:"
      , PP.ppString (showConwayTxBalance uePParams ueCertState utxosUtxo sig)
      , "initial TotalAda:" <+> PP.ppString (showExpr $ totalAda st)
      , "final TotalAda:  " <+> case st' of
          Right (x, _) -> PP.ppString (showExpr $ totalAda x)
          Left _ -> "N/A"
      , mempty
      , "Spec:"
      , PP.ppString
          ( either show T.unpack . runSpecTransM ctx $
              Agda.utxoDebug externalFunctions
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          )
      ]
