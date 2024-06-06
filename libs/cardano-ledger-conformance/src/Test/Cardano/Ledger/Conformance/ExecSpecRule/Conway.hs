{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway () where

import Cardano.Ledger.BaseTypes (Inject (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams)
import Cardano.Ledger.Conway.Governance (EnactState)
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Constrained
import Control.DeepSeq (NFData)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  computationResultToEither,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (
  GovProceduresSpecTransCtx,
  OpaqueErrorString (..),
 )
import Test.Cardano.Ledger.Constrained.Conway (
  IsConwayUniv,
  govEnvSpec,
  govProceduresSpec,
  govProposalsSpec,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )

data ConwayGovExecContext era
  = ConwayGovExecContext
      (GovProceduresSpecTransCtx (EraCrypto era))
      (EnactState era)
  deriving (Generic)

deriving instance EraPParams era => Eq (ConwayGovExecContext era)

deriving instance EraPParams era => Show (ConwayGovExecContext era)

instance HasSimpleRep (ConwayGovExecContext era)

instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (EnactState era)
  ) =>
  HasSpec fn (ConwayGovExecContext era)

instance c ~ EraCrypto era => Inject (ConwayGovExecContext era) (GovProceduresSpecTransCtx c) where
  inject (ConwayGovExecContext x _) = x

instance Inject (ConwayGovExecContext era) (EnactState era) where
  inject (ConwayGovExecContext _ x) = x

instance
  ( NFData (SpecRep (ConwayGovPredFailure Conway))
  , IsConwayUniv fn
  ) =>
  ExecSpecRule fn "GOV" Conway
  where
  type ExecContext fn "GOV" Conway = ConwayGovExecContext Conway

  environmentSpec = govEnvSpec

  stateSpec = govProposalsSpec

  signalSpec = govProceduresSpec

  execContextSpec = TrueSpec

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.govStep env st sig

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  environmentSpec = utxoEnvSpec

  stateSpec = utxoStateSpec

  signalSpec env st = utxoTxSpec env st <> constrained agdaConstraints
    where
      agdaConstraints :: Term fn (AlonzoTx Conway) -> Pred fn
      agdaConstraints tx = match @fn tx $ \txBody _ _ _ _ ->
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
           _ctbTreasuryDonation
           _bftbRequests
           _bftbFulfills
           _bftbRequiredTxs ->
              match ctbOutputs $
                \outs -> forAll outs $
                  \x -> match x $
                    \txOut _ -> match txOut $
                      \_ _ dat _ ->
                        caseOn
                          dat
                          (branch $ const True)
                          (branch $ const True)
                          (branch $ const False)

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.utxoStep env st sig
