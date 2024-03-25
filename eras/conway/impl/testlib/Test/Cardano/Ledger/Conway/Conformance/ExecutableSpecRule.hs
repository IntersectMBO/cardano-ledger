{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Test.Cardano.Ledger.Conway.Conformance.ExecutableSpecRule () where

import Test.Cardano.Ledger.Conway.Conformance.SpecTranslate ()
import Test.Cardano.Ledger.Conformance (ExecutableSpecRule (..), SpecTranslate (..), computationResultToEither)
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Governance (GovProcedures, Proposals)
import Control.DeepSeq (NFData)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure, GovEnv)
import Test.Cardano.Ledger.Common (ToExpr)
import Test.Cardano.Ledger.Conway.Constrained.Spec.Gov (govEnvSpec, govProposalsSpec, govProceduresSpec)
import Test.Cardano.Ledger.Conway.Constrained.Instances (IsConwayUniv)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conway.Constrained.Spec.Utxo (utxoStateSpec, utxoEnvSpec, utxoTxSpec)
import Test.Cardano.Ledger.Conway.TreeDiff ()
import Data.Bifunctor (Bifunctor(..))
import qualified Data.List.NonEmpty as NE

instance
  ( NFData (SpecRep (Proposals Conway))
  , NFData (SpecRep (ConwayGovPredFailure Conway))
  , NFData (TestRep (ConwayGovPredFailure Conway))
  , NFData (TestRep (Proposals Conway))
  , SpecTranslate (ConwayGovPredFailure Conway)
  , SpecTranslate (GovEnv Conway)
  , SpecTranslate (GovProcedures Conway)
  , SpecTranslate (Proposals Conway)
  , Eq (TestRep (ConwayGovPredFailure Conway))
  , Eq (TestRep (Proposals Conway))
  , ToExpr (TestRep (ConwayGovPredFailure Conway))
  , ToExpr (TestRep (Proposals Conway))
  , IsConwayUniv fn
  ) =>
  ExecutableSpecRule fn "GOV" Conway
  where

  environmentSpec = govEnvSpec

  stateSpec = govProposalsSpec

  signalSpec = govProceduresSpec

  runAgdaRule = undefined

instance
  ( IsConwayUniv fn
  ) =>
  ExecutableSpecRule fn "UTXO" Conway
  where
  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec = utxoTxSpec

  runAgdaRule env st sig =
    first (const $ () NE.:| []) . computationResultToEither $ Agda.utxoStep env st sig


