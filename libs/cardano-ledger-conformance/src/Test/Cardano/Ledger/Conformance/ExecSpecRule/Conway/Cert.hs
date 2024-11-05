{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Cert (nameTxCert) where

import Cardano.Ledger.Address (RewardAccount)
import Cardano.Ledger.BaseTypes (Inject)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.TxCert (ConwayTxCert (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Constrained (lit)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (nameDelegCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.GovCert (nameGovCert)
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Pool (namePoolCert)
import Test.Cardano.Ledger.Constrained.Conway

instance
  ( IsConwayUniv fn
  , Inject (ConwayCertExecContext Conway) (Map (RewardAccount StandardCrypto) Coin)
  ) =>
  ExecSpecRule fn "CERT" Conway
  where
  type ExecContext fn "CERT" Conway = ConwayCertExecContext Conway
  environmentSpec _ = certEnvSpec
  stateSpec ctx _ = certStateSpec (lit $ ccecDelegatees ctx)
  signalSpec _ = txCertSpec
  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.certStep env st sig

  classOf = Just . nameTxCert

nameTxCert :: ConwayTxCert Conway -> String
nameTxCert (ConwayTxCertDeleg x) = nameDelegCert x
nameTxCert (ConwayTxCertPool x) = namePoolCert x
nameTxCert (ConwayTxCertGov x) = nameGovCert x
