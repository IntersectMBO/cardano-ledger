{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Conformance.ExecSpecRule.Conway () where

import Cardano.Ledger.BaseTypes (Inject (..), StrictMaybe (..))
import Cardano.Ledger.Conway (Conway)
import Cardano.Ledger.Conway.Core (Era (..), EraPParams (..), PParamsUpdate)
import Cardano.Ledger.Conway.Governance (
  EnactState,
  GovProcedures (..),
  ProposalProcedure,
  Proposals,
  ensPrevGovActionIdsL,
  pRootsL,
  toPrevGovActionIds,
 )
import Cardano.Ledger.Conway.PParams (THKD (..))
import Cardano.Ledger.Conway.Rules (ConwayGovPredFailure)
import Cardano.Ledger.Conway.Tx (AlonzoTx)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.TxIn (TxId)
import Constrained
import Control.Monad.Identity (Identity)
import Data.Bifunctor (Bifunctor (..))
import Data.Default.Class (Default (..))
import Data.Foldable (Foldable (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.OSet.Strict as OSet
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro ((&), (.~), (^.))
import qualified Lib as Agda
import Test.Cardano.Ledger.Conformance (
  ExecSpecRule (..),
  SpecTranslate (..),
  checkConformance,
  computationResultToEither,
  runConformance,
  runSpecTransM,
 )
import Test.Cardano.Ledger.Conformance.SpecTranslate.Conway (
  OpaqueErrorString (..),
 )
import Test.Cardano.Ledger.Constrained.Conway (
  IsConwayUniv,
  ProposalsSplit,
  genProposalsSplit,
  govEnvSpec,
  govProceduresSpec,
  govProposalsSpec,
  utxoEnvSpec,
  utxoStateSpec,
  utxoTxSpec,
 )
import Test.Cardano.Ledger.Conway.ImpTest (impAnn, logEntry)
import Test.Cardano.Ledger.Imp.Common hiding (forAll, prop)

data ConwayGovTransContext era
  = ConwayGovTransContext
      (EnactState era)
      (TxId (EraCrypto era))
      (Proposals era)
  deriving (Generic)

deriving instance EraPParams era => Eq (ConwayGovTransContext era)

deriving instance EraPParams era => Show (ConwayGovTransContext era)

instance
  ( Era era
  , ToExpr (PParamsHKD Identity era)
  , ToExpr (PParamsHKD StrictMaybe era)
  ) =>
  ToExpr (ConwayGovTransContext era)

instance HasSimpleRep (ConwayGovTransContext era)

instance
  ( IsConwayUniv fn
  , EraPParams era
  , HasSpec fn (EnactState era)
  , HasSpec fn (Proposals era)
  ) =>
  HasSpec fn (ConwayGovTransContext era)

instance Inject (ConwayGovTransContext era) (EnactState era) where
  inject (ConwayGovTransContext x _ _) = x

instance EraCrypto era ~ c => Inject (ConwayGovTransContext era) (TxId c) where
  inject (ConwayGovTransContext _ x _) = x

instance EraCrypto era ~ c => Inject (ConwayGovTransContext era) (Proposals era) where
  inject (ConwayGovTransContext _ _ x) = x

agdaCompatiblePPU :: IsConwayUniv fn => Term fn (PParamsUpdate Conway) -> Pred fn
agdaCompatiblePPU ppup =
  match ppup $
    \cppMinFeeA
     cppMinFeeB
     cppMaxBBSize
     cppMaxTxSize
     cppMaxBHSize
     cppKeyDeposit
     cppPoolDeposit
     cppEMax
     cppNOpt
     cppA0
     cppRho
     cppTau
     _cppProtocolVersion
     cppMinPoolCost
     cppCoinsPerUTxOByte
     cppCostModels
     cppPrices
     cppMaxTxExUnits
     cppMaxBlockExUnits
     cppMaxValSize
     cppCollateralPercentage
     cppMaxCollateralInputs
     cppPoolVotingThresholds
     cppDRepVotingThresholds
     cppCommitteeMinSize
     cppCommitteeMaxTermLength
     cppGovActionLifetime
     cppGovActionDeposit
     cppDRepDeposit
     cppDRepActivity
     cppMinFeeRefScriptCostPerByte ->
        -- TODO enable pparam updates once they're properly
        -- implemented in the spec
        mconcat
          [ isModified cppMinFeeA
          , isUnmodified cppMinFeeB
          , isUnmodified cppMaxBBSize
          , isUnmodified cppMaxTxSize
          , isUnmodified cppMaxBHSize
          , isUnmodified cppKeyDeposit
          , isUnmodified cppPoolDeposit
          , isUnmodified cppEMax
          , isUnmodified cppNOpt
          , isUnmodified cppA0
          , isUnmodified cppRho
          , isUnmodified cppTau
          , isUnmodified cppMinPoolCost
          , isUnmodified cppCoinsPerUTxOByte
          , isUnmodified cppCostModels
          , isUnmodified cppPrices
          , isUnmodified cppMaxTxExUnits
          , isUnmodified cppMaxBlockExUnits
          , isUnmodified cppMaxValSize
          , isUnmodified cppCollateralPercentage
          , isUnmodified cppMaxCollateralInputs
          , isUnmodified cppPoolVotingThresholds
          , isUnmodified cppDRepVotingThresholds
          , isUnmodified cppCommitteeMinSize
          , isUnmodified cppCommitteeMaxTermLength
          , isUnmodified cppGovActionLifetime
          , isUnmodified cppGovActionDeposit
          , isUnmodified cppDRepDeposit
          , isUnmodified cppDRepActivity
          , isUnmodified cppMinFeeRefScriptCostPerByte
          ]
  where
    isUnmodified ::
      ( HasSpec fn a
      , Typeable gs
      , IsNormalType a
      , IsConwayUniv fn
      ) =>
      Term fn (THKD gs StrictMaybe a) ->
      Pred fn
    isUnmodified x =
      caseOn
        x
        (branch $ const True)
        (branch $ const False)
    isModified ::
      ( HasSpec fn a
      , Typeable gs
      , IsNormalType a
      , IsConwayUniv fn
      ) =>
      Term fn (THKD gs StrictMaybe a) ->
      Pred fn
    isModified x =
      caseOn
        x
        (branch $ const False)
        (branch $ const True)

agdaCompatibleProposal ::
  IsConwayUniv fn =>
  Term fn (ProposalProcedure Conway) ->
  Pred fn
agdaCompatibleProposal prop =
  match prop $ \_ _ govAction _ ->
    caseOn
      govAction
      (branch $ \_ ppup _ -> agdaCompatiblePPU ppup)
      (branch $ \_ _ -> True)
      (branch $ \_ _ -> True)
      (branch $ const True)
      (branch $ \_ _ _ _ -> True)
      (branch $ \_ _ -> True)
      (branch $ const True)

instance
  ( NFData (SpecRep (ConwayGovPredFailure Conway))
  , IsConwayUniv fn
  ) =>
  ExecSpecRule fn "GOV" Conway
  where
  type ExecContext fn "GOV" Conway = (TxId StandardCrypto, ProposalsSplit)

  environmentSpec _ = govEnvSpec

  stateSpec (_, propSplit) env = govProposalsSpec env <> constrained agdaConstraints
    where
      agdaConstraints :: Term fn (Proposals Conway) -> Pred fn
      agdaConstraints props =
        fold
          [ match @fn props $
              \ppups _ _ _ _ ->
                [ match ppups $ \_ ppupForest ->
                    forAll ppupForest $ \ppupTree ->
                      forAll' ppupTree $ \gas _ ->
                        match gas $ \_ _ _ _ prop _ _ -> agdaCompatibleProposal prop
                ]
          , genHint propSplit props
          ]

  signalSpec _ env st = govProceduresSpec env st <> constrained agdaConstraints
    where
      agdaConstraints :: Term fn (GovProcedures Conway) -> Pred fn
      agdaConstraints procs = match @fn procs $ \_ props ->
        forAll props agdaCompatibleProposal

  genExecContext = (,) <$> arbitrary <*> genProposalsSplit 20

  runAgdaRule env st sig =
    first (\e -> OpaqueErrorString (T.unpack e) NE.:| [])
      . computationResultToEither
      $ Agda.govStep env st sig

  translateInputs env st sig (txId, _) = do
    let
      modifiedEnactState =
        def
          & ensPrevGovActionIdsL .~ toPrevGovActionIds (st ^. pRootsL)
      modifiedCtx = ConwayGovTransContext modifiedEnactState txId st
    logEntry $ "modifiedCtx:\n" <> showExpr modifiedCtx
    agdaEnv <-
      impAnn "Translating the environment" . expectRight . runSpecTransM modifiedCtx $ toSpecRep env
    logEntry $ "agdaEnv:\n" <> showExpr agdaEnv
    agdaSt <- impAnn "Translating the state" . expectRight . runSpecTransM modifiedCtx $ toSpecRep st
    logEntry $ "agdaSt:\n" <> showExpr agdaSt
    agdaSig <- impAnn "Translating the signal" . expectRight . runSpecTransM modifiedCtx $ toSpecRep sig
    logEntry $ "agdaSig:\n" <> showExpr agdaSig
    pure (agdaEnv, agdaSt, agdaSig)

  testConformance env st sig = property $ do
    (implResTest, agdaResTest) <- runConformance @"GOV" @fn @Conway env st sig
    checkConformance @"GOV" implResTest agdaResTest
    let numInputProps = OSet.size $ gpProposalProcedures sig
    pure $ label ("n input proposals = " <> show numInputProps) ()

instance
  forall fn.
  IsConwayUniv fn =>
  ExecSpecRule fn "UTXO" Conway
  where
  environmentSpec _ = utxoEnvSpec

  stateSpec _ = utxoStateSpec

  signalSpec _ env st = utxoTxSpec env st <> constrained agdaConstraints
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
                \outs -> forAll' outs $
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
