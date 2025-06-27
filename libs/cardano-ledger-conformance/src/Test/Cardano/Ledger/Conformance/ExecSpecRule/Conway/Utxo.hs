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
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
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
import Test.Cardano.Ledger.Generic.Instances ()
import Test.Cardano.Ledger.Generic.TxGen (genAlonzoTx)

genUtxoExecContext :: Gen (UtxoExecContext ConwayEra)
genUtxoExecContext = do
  ueSlot <- arbitrary
  let
    genSize =
      GenSize.small
        { invalidScriptFreq = 0 -- TODO make the test work with invalid scripts
        , regCertFreq = 0
        , delegCertFreq = 0
        }
  ((uecUTxO, uecTx), gs) <- runGenRS genSize $ genAlonzoTx ueSlot
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
      , PP.pretty (showConwayTxBalance uePParams ueCertState utxosUtxo sig)
      , "initial TotalAda:" <+> PP.pretty (showExpr $ totalAda st)
      , "final TotalAda:  " <+> case st' of
          Right (x, _) -> PP.pretty (showExpr $ totalAda x)
          Left _ -> "N/A"
      , mempty
      , "Spec:"
      , PP.pretty
          ( either show T.unpack . runSpecTransM ctx $
              Agda.utxoDebug externalFunctions
                <$> toSpecRep env
                <*> toSpecRep st
                <*> toSpecRep sig
          )
      ]
