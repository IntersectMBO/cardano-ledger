{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
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
import Cardano.Ledger.Conway.Core (EraTx (..))
#if __GLASGOW_HASKELL__ < 914
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
#endif
import Cardano.Ledger.Conway.UTxO (getConwayWitsVKeyNeeded)
import Cardano.Ledger.Shelley.LedgerState (UTxOState (..))
#if __GLASGOW_HASKELL__ < 914
import Cardano.Ledger.TxIn (TxId)
#endif
import Control.State.Transition.Extended (TRC (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import qualified Data.Text as T
import Lens.Micro ((^.))
import qualified MAlonzo.Code.Ledger.Foreign.API as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  runFromAgdaFunction,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base (externalFunctions)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo ()
import Test.Cardano.Ledger.Constrained.Conway (
  UtxoExecContext,
 )
import Test.Cardano.Ledger.Conway.TreeDiff (showExpr)
import Test.Cardano.Ledger.Shelley.Utils (runSTS)

instance
#if __GLASGOW_HASKELL__ < 914
  -- These constraints are REQUIRED for ghc < 9.14 but REDUNDANT for ghc >= 9.14
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/26381#note_637863
  SpecTranslate TxId (ConwayTxCert ConwayEra) =>
#endif
  ExecSpecRule "UTXOW" ConwayEra
  where
  type ExecContext "UTXOW" ConwayEra = UtxoExecContext ConwayEra

  runAgdaRule = runFromAgdaFunction (Agda.utxowStep externalFunctions)

  extraInfo globals ctx trc@(TRC (env, st, sig)) _ =
    let
      result =
        either show T.unpack . runSpecTransM ctx $
          Agda.utxowDebug externalFunctions
            <$> toSpecRep env
            <*> toSpecRep st
            <*> toSpecRep sig
      stFinal = first (T.pack . show) $ runSTS @"UTXO" @ConwayEra globals env st sig
      utxoInfo = extraInfo @"UTXO" @ConwayEra globals ctx (coerce trc) stFinal
     in
      PP.vcat
        [ "UTXOW"
        , "Impl:"
        , "witsVKeyNeeded"
        , PP.pretty . showExpr $
            getConwayWitsVKeyNeeded @ConwayEra (utxosUtxo st) (sig ^. bodyTxL)
        , "witsVKeyHashes"
        , "Spec:"
        , PP.pretty result
        , mempty
        , "UTXO"
        , utxoInfo
        ]
