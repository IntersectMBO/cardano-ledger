{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Deleg (
  ConwayDelegExecContext (..),
  nameDelegCert,
) where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Conway
import Cardano.Ledger.Conway.Core (Era (..))
import Cardano.Ledger.Conway.TxCert (ConwayDelegCert (..))
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Keys (KeyRole (Staking))
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Lib as Agda
import Test.Cardano.Ledger.Common (Arbitrary (..), NFData, ToExpr)
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Core ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Base ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Cert ()
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway.Deleg ()
import Test.Cardano.Ledger.Constrained.Conway

newtype ConwayDelegExecContext era = ConwayDelegExecContext
  { cdecDRepDelegs :: Set (Credential 'Staking (EraCrypto era))
  }
  deriving (Generic, Eq, Show)

instance Era era => Arbitrary (ConwayDelegExecContext era) where
  arbitrary =
    ConwayDelegExecContext
      <$> arbitrary

instance Era era => EncCBOR (ConwayDelegExecContext era) where
  encCBOR (ConwayDelegExecContext x) = encCBOR x

instance
  c ~ EraCrypto era =>
  Inject (ConwayDelegExecContext era) (Set (Credential 'Staking c))
  where
  inject = cdecDRepDelegs

instance Era era => ToExpr (ConwayDelegExecContext era)

instance Era era => NFData (ConwayDelegExecContext era)

instance IsConwayUniv fn => ExecSpecRule fn "DELEG" Conway where
  type ExecContext fn "DELEG" Conway = ConwayDelegExecContext Conway

  genExecContext = arbitrary

  environmentSpec _ = delegEnvSpec

  stateSpec _ _ = certStateSpec

  signalSpec _ = conwayDelegCertSpec

  runAgdaRule env (Agda.MkCertState dState pState vState) sig =
    bimap
      (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      (\dState' -> Agda.MkCertState dState' pState vState)
      . computationResultToEither
      $ Agda.delegStep env dState sig

  classOf = Just . nameDelegCert

nameDelegCert :: ConwayDelegCert c -> String
nameDelegCert ConwayRegCert {} = "RegKey"
nameDelegCert ConwayUnRegCert {} = "UnRegKey"
nameDelegCert ConwayDelegCert {} = "DelegateWithKey"
nameDelegCert ConwayRegDelegCert {} = "RegK&DelegateWithKey"
