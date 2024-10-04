{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxow () where

import Cardano.Ledger.Conway.Core (AlonzoEraScript (..), AsItem, AsIx, Era, EraRule, EraTxCert (..))
import Cardano.Ledger.Conway.Rules (ConwayUtxowPredFailure, PredicateFailure)
import Test.Cardano.Ledger.Conformance (OpaqueErrorString (..), SpecTranslate (..))
import Test.Cardano.Ledger.Conway.TreeDiff (ToExpr, showExpr)

import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxo ()

instance
  ( Era era
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (TxCert era)
  ) =>
  SpecTranslate ctx (ConwayUtxowPredFailure era)
  where
  type SpecRep (ConwayUtxowPredFailure era) = OpaqueErrorString

  toSpecRep = pure . OpaqueErrorString . showExpr
