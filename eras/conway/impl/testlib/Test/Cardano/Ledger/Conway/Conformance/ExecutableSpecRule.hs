{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}

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
import Constrained
import Cardano.Ledger.Conway.Tx (AlonzoTx)

instance
  ( NFData (SpecRep (Proposals Conway))
  , NFData (SpecRep (ConwayGovPredFailure Conway))
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

  runAgdaRule env st sig =
    first (const $ () NE.:| []) . computationResultToEither $ Agda.govStep env st sig

instance
  forall fn.
  ( IsConwayUniv fn
  ) =>
  ExecutableSpecRule fn "UTXO" Conway
  where
  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec env st = utxoTxSpec env st <> constrained agdaConstraints
    where
      agdaConstraints :: Term fn (AlonzoTx Conway) -> Pred fn
      agdaConstraints tx = match @fn tx $ \txBody _ _ _ ->
        match txBody $
          \_ctbSpendInputs
           _ctbCollateralInputs
           _ctbReferenceInputs
           ctbOutputs
           _ctbCollateralReturn
           _ctbTotalCollateral
           _ctbCerts
           _ctbWithdrawals
           _ctbTxfee
           _ctbVldt
           _ctbReqSignerHashes
           _ctbMint
           _ctbScriptIntegrityHash
           _ctbAdHash
           _ctbTxNetworkId
           _ctbVotingProcedures
           _ctbProposalProcedures
           _ctbCurrentTreasuryValue
           _ctbTreasuryDonation ->
             match ctbOutputs $
               \outs -> forAll outs $
                 \x -> match x $
                   \txOut _ -> match txOut $
                     \_ _ dat _ -> caseOn dat
                       (branch $ const True)
                       (branch $ const True)
                       (branch $ const False)

  runAgdaRule env st sig =
    first (const $ () NE.:| []) . computationResultToEither $ Agda.utxoStep env st sig


