{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Conway.UTxO (getConwayWitsVKeyNeeded)
import Cardano.Ledger.Core (EraTx (..))
import Cardano.Ledger.State
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Text as T
import Lens.Micro ((^.))
import qualified Lib as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate,
  runSpecTransM,
  showOpaqueErrorString,
  toSpecRep,
  unComputationResult,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo (genUtxoExecContext)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  ConwayTxBodyTransContext,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxow ()
import Test.Cardano.Ledger.Constrained.Conway (
  IsConwayUniv,
  UtxoExecContext,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import qualified Test.Cardano.Ledger.Generic.PrettyCore as PP
import Test.Cardano.Ledger.Shelley.Utils (runSTS)
import Test.Cardano.Ledger.TreeDiff (showExpr)

instance
  ( IsConwayUniv fn
  , SpecTranslate ConwayTxBodyTransContext (ConwayTxCert ConwayEra)
  ) =>
  ExecSpecRule fn "UTXOW" ConwayEra
  where
  type ExecContext fn "UTXOW" ConwayEra = UtxoExecContext ConwayEra

  genExecContext = genUtxoExecContext
  environmentSpec = utxoEnvSpec
  stateSpec = utxoStateSpec
  signalSpec ctx _ _ = utxoTxSpec ctx
  runAgdaRule env st sig =
    unComputationResult $ Agda.utxowStep externalFunctions env st sig
  extraInfo globals ctx env st sig _ =
    let
      result =
        either show T.unpack . runSpecTransM ctx $
          Agda.utxowDebug externalFunctions
            <$> toSpecRep env
            <*> toSpecRep st
            <*> toSpecRep sig
      stFinal = first showOpaqueErrorString $ runSTS @"UTXO" @ConwayEra globals env st sig
      utxoInfo = extraInfo @fn @"UTXO" @ConwayEra globals ctx env st sig stFinal
     in
      PP.vcat
        [ "UTXOW"
        , "Impl:"
        , "witsVKeyNeeded"
        , PP.ppString . showExpr $
            getConwayWitsVKeyNeeded @ConwayEra (utxosUtxo st) (sig ^. bodyTxL)
        , "witsVKeyHashes"
        , "Spec:"
        , PP.ppString result
        , mempty
        , "UTXO"
        , utxoInfo
        ]
