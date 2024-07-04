{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg () where

import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert)
import Cardano.Ledger.Crypto
import Constrained
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  environmentSpec _ = delegEnvSpec

  stateSpec _ _ = dStateSpec

  signalSpec _ env st =
    delegCertSpec env st
      <> constrained disableRegCertsDelegCert

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.delegStep env st sig

disableRegCertsDelegCert ::
  ( IsConwayUniv fn
  , Crypto c
  ) =>
  Term fn (ConwayDelegCert c) ->
  Pred fn
disableRegCertsDelegCert delegCert =
  (caseOn delegCert)
    (branch $ \_ _ -> False)
    (branch $ \_ _ -> True)
    (branch $ \_ _ -> True)
    (branch $ \_ _ _ -> True)
