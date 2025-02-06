{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Gov () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Credential (Credential)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common
import GHC.Generics (Generic)
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Binary.Coders (encode, Encode (..), (!>))
import Data.Set (Set)

instance
  Inject
    (ConwayGovExecContext ConwayEra)
    (Set (Credential Staking))
  where
  inject = cgecStakeDelegs

instance
  Inject
    (ConwayGovExecContext ConwayEra)
    (EnactState ConwayEra)
  where
  inject = cgecEnactState

data ConwayGovExecContext era = ConwayGovExecContext
  { cgecEnactState :: EnactState era
  , cgecStakeDelegs :: Set (Credential 'Staking)
  }
  deriving (Generic)
  deriving anyclass (NFData)

instance EraPParams era => EncCBOR (ConwayGovExecContext era) where
  encCBOR x@(ConwayGovExecContext _ _) =
    let ConwayGovExecContext {..} = x
     in encode $
       Rec ConwayGovExecContext
         !> To cgecEnactState
         !> To cgecStakeDelegs

instance (Era era, ToExpr (PParams era)) => ToExpr (ConwayGovExecContext era)

instance
  ( NFData (SpecRep (ConwayGovPredFailure ConwayEra))
  , IsConwayUniv fn
  ) =>
  ExecSpecRule fn "GOV" ConwayEra
  where
  type ExecContext fn "GOV" ConwayEra = ConwayGovExecContext ConwayEra

  environmentSpec _ = govEnvSpec

  stateSpec _ = govProposalsSpec

  signalSpec _ = govProceduresSpec

  genExecContext =
    ConwayGovExecContext
      <$> arbitrary
      <*> arbitrary

  runAgdaRule env st sig = unComputationResult $ Agda.govStep env st sig

  translateInputs env st sig ctx = do
    agdaEnv <- expectRight $ runSpecTransM ctx $ toSpecRep env
    agdaSt <- expectRight $ runSpecTransM ctx $ toSpecRep st
    agdaSig <- expectRight $ runSpecTransM ctx $ toSpecRep sig
    pure (agdaEnv, agdaSt, agdaSig)
