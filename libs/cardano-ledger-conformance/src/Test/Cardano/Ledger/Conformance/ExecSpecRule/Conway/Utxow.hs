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

import Cardano.Crypto.DSIGN.Class (SignedDSIGN (..), verifySignedDSIGN)
import Cardano.Crypto.Hash (ByteString, Hash)
import Cardano.Ledger.Conway (Conway, ConwayEra)
import Cardano.Ledger.Conway.TxCert (ConwayTxCert)
import Cardano.Ledger.Crypto (Crypto (..), StandardCrypto)
import Cardano.Ledger.Keys (VKey (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  OpaqueErrorString (..),
  SpecTranslate,
  computationResultToEither,
  integerToHash,
 )
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Utxo (genUtxoExecContext)
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base (
  ConwayTxBodyTransContext,
  signatureFromInteger,
  vkeyFromInteger,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Utxow ()
import Test.Cardano.Ledger.Constrained.Conway (
  IsConwayUniv,
  UtxoExecContext,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )

externalFunctions :: Agda.ExternalFunctions
externalFunctions = Agda.MkExternalFunctions {..}
  where
    extIsSigned vk ser sig =
      isRight $
        verifySignedDSIGN
          @(DSIGN StandardCrypto)
          @(Hash (HASH StandardCrypto) ByteString)
          ()
          vkey
          hash
          signature
      where
        vkey =
          unVKey @_ @StandardCrypto
            . fromMaybe (error "Failed to convert an Agda VKey to a Haskell VKey")
            $ vkeyFromInteger vk
        hash =
          fromMaybe
            (error $ "Failed to get hash from integer:\n" <> show ser)
            $ integerToHash ser
        signature =
          SignedDSIGN
            . fromMaybe
              (error "Failed to decode the signature")
            $ signatureFromInteger sig

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
