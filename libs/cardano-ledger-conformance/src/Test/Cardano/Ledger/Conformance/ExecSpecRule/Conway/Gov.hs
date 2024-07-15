{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Gov () where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Governance
import Cardano.Ledger.Conway.Rules
import Cardano.Ledger.Crypto
import Cardano.Ledger.TxIn (TxId)
import Data.Bifunctor (first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Lens.Micro
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance
import Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway.Base ()
import Test.Cardano.Ledger.Constrained.Conway
import Test.Cardano.Ledger.Conway.Arbitrary ()
import Test.Cardano.Ledger.Imp.Common

instance
  Inject
    (TxId StandardCrypto, Proposals Conway, EnactState Conway)
    (EnactState Conway)
  where
  inject (_, _, x) = x

instance
  ( NFData (SpecRep (ConwayGovPredFailure Conway))
  , IsConwayUniv fn
  ) =>
  ExecSpecRule fn "GOV" Conway
  where
  type
    ExecContext fn "GOV" Conway =
      (TxId StandardCrypto, ProposalsSplit, EnactState Conway)

  environmentSpec _ = govEnvSpec

  stateSpec _ = govProposalsSpec

  signalSpec _ = govProceduresSpec

  genExecContext = do
    txId <- arbitrary
    proposalsSplit <- genProposalsSplit 50
    enactState <- arbitrary
    pure
      ( txId
      , proposalsSplit
      , enactState
      )

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.govStep env st sig

  translateInputs env@GovEnv {gePParams} st sig (txId, _proposalsSplit, enactState) = do
    agdaEnv <- expectRight $ runSpecTransM ctx $ toSpecRep env
    agdaSt <- expectRight $ runSpecTransM ctx $ toSpecRep st
    agdaSig <- expectRight $ runSpecTransM ctx $ toSpecRep sig
    pure (agdaEnv, agdaSt, agdaSig)
    where
      ctx =
        ( txId
        , st
        , enactState
            & ensPrevGovActionIdsL .~ toPrevGovActionIds (st ^. pRootsL)
            & ensProtVerL .~ (gePParams ^. ppProtocolVersionL)
        )
