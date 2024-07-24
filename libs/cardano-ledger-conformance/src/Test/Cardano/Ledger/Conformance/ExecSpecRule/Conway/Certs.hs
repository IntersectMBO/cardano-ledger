{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Certs () where

import Cardano.Ledger.Conway
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway ()
import Test.Cardano.Ledger.Constrained.Conway

instance
  IsConwayUniv fn =>
  ExecSpecRule fn "CERTS" Conway
  where
  type ExecEnvironment fn "CERTS" Conway = CertsExecEnv Conway

  environmentSpec _ = certsExecEnvSpec

  stateSpec _ _ = certStateSpec

  signalSpec _ _ _ = txCertsSpec

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certsStep env st sig
