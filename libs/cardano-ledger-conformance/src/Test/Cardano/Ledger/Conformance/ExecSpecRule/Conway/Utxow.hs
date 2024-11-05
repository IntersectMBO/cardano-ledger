{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxow () where

import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Crypto (StandardCrypto)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  SpecTranslate,
  computationResultToEither,
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

instance
  ( IsConwayUniv fn
  , SpecTranslate (ConwayTxBodyTransContext StandardCrypto) (ConwayTxCert (ConwayEra StandardCrypto))
  ) =>
  ExecSpecRule fn "UTXOW" Conway
  where
  type ExecContext fn "UTXOW" Conway = UtxoExecContext Conway

  genExecContext = genUtxoExecContext
  environmentSpec = utxoEnvSpec
  stateSpec = utxoStateSpec
  signalSpec ctx _ _ = utxoTxSpec ctx
  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxowStep externalFunctions env st sig
