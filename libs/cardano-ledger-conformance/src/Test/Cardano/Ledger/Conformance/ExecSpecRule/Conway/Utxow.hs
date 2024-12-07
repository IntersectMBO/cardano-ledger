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

import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Crypto (StandardCrypto)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import qualified Prettyprinter as PP
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  SpecTranslate,
  computationResultToEither,
  runSpecTransM,
  toSpecRep,
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
  extraInfo globals ctx env st sig _ =
    let
      result =
        either show T.unpack . runSpecTransM ctx $
          Agda.utxowDebug externalFunctions
            <$> toSpecRep env
            <*> toSpecRep st
            <*> toSpecRep sig
      stFinal = runSTS @"UTXO" @Conway globals env st sig
      utxoInfo = extraInfo @fn @"UTXO" @Conway globals ctx env st sig stFinal
     in
      PP.vcat
        [ "UTXOW"
        , PP.ppString result
        , mempty
        , "UTXO"
        , utxoInfo
        ]
