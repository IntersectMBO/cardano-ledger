{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow () where

import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate, runFromAgdaFunction,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  ConwayTxBodyTransContext,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxow ()
import Test.Cardano.Ledger.Constrained.Conway (
  UtxoExecContext,
 )

instance
  SpecTranslate ConwayTxBodyTransContext (ConwayTxCert ConwayEra) =>
  ExecSpecRule "UTXOW" ConwayEra
  where
  type ExecContext "UTXOW" ConwayEra = UtxoExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction (Agda.utxowStep externalFunctions)
  --extraInfo globals ctx env st sig _ =
  --  let
  --    result =
  --      either show T.unpack . runSpecTransM ctx $
  --        Agda.utxowDebug externalFunctions
  --          <$> toSpecRep env
  --          <*> toSpecRep st
  --          <*> toSpecRep sig
  --    stFinal = first showOpaqueErrorString $ runSTS @"UTXO" @ConwayEra globals env st sig
  --    utxoInfo = extraInfo @"UTXO" @ConwayEra globals ctx env st sig stFinal
  --   in
  --    PP.vcat
  --      [ "UTXOW"
  --      , "Impl:"
  --      , "witsVKeyNeeded"
  --      , PP.pretty . showExpr $
  --          getConwayWitsVKeyNeeded @ConwayEra (utxosUtxo st) (sig ^. bodyTxL)
  --      , "witsVKeyHashes"
  --      , "Spec:"
  --      , PP.pretty result
  --      , mempty
  --      , "UTXO"
  --      , utxoInfo
  --      ]
