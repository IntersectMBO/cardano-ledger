{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovProposals (
  module Cardano.Ledger.SCLS.Namespace.GovProposals.V0,
) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
import Cardano.Ledger.Coin (Coin (..), CompactForm)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionState (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
 )
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
import Cardano.Ledger.Core (PParamsUpdate (..))
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.SCLS.Common
import Cardano.Ledger.SCLS.Namespace.GovProposals.V0
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

instance IsCanonicalGovActionIx GovActionIx where
  mkCanonicalGovActionIx (GovActionIx ix) = CanonicalGovActionIx ix
  fromCanonicalGovActionIx (CanonicalGovActionIx ix) = GovActionIx ix

instance IsCanonicalGovActionId GovActionId where
  mkCanonicalGovActionId GovActionId {..} =
    CanonicalGovActionId
      { gaidGovActionIx = mkCanonicalGovActionIx gaidGovActionIx
      , ..
      }
  fromCanonicalGovActionId CanonicalGovActionId {..} =
    GovActionId
      { gaidGovActionIx = fromCanonicalGovActionIx gaidGovActionIx
      , ..
      }

instance IsCanonicalProposalProcedure (ProposalProcedure ConwayEra) where
  mkCanonicalProposalProcedure ProposalProcedure {..} =
    CanonicalProposalProcedure
      { pProcGovAction = mkCanonicalGovAction pProcGovAction
      , pProcReturnAddr = mkCanonicalRewardAccount pProcReturnAddr
      , pProcDeposit = mkCanonicalCoin pProcDeposit
      , ..
      }
  fromCanonicalProposalProcedure CanonicalProposalProcedure {..} =
    ProposalProcedure
      { pProcGovAction = fromCanonicalGovAction pProcGovAction
      , pProcReturnAddr = fromCanonicalRewardAccount pProcReturnAddr
      , pProcDeposit = fromCanonicalCoin pProcDeposit
      , ..
      }

instance IsCanonicalPurposeId (GovPurposeId p) where
  mkCanonicalPurposeId (GovPurposeId p) = CanonicalPurposeId (mkCanonicalGovActionId p)
  fromCanonicalPurposeId (CanonicalPurposeId p) = GovPurposeId (fromCanonicalGovActionId p)

instance IsCanonicalPParamsUpdate (PParamsUpdate ConwayEra) where
  mkCanonicalPParamsUpdate (PParamsUpdate ConwayPParams {..}) =
    CanonicalPParamsUpdate
      { ucppA0 = unTHKD cppA0
      , ucppTxFeePerByte = fmap (mkCanonicalCoin . unCoinPerByte) $ unTHKD cppTxFeePerByte
      , ucppTxFeeFixed = fmap mkCanonicalCoin $ unTHKD cppTxFeeFixed
      , ucppMaxBBSize = unTHKD cppMaxBBSize
      , ucppMaxTxSize = unTHKD cppMaxTxSize
      , ucppMaxBHSize = unTHKD cppMaxBHSize
      , ucppKeyDeposit = fmap mkCanonicalCoin $ unTHKD cppKeyDeposit
      , ucppPoolDeposit = fmap mkCanonicalCoin $ unTHKD cppPoolDeposit
      , ucppEMax = unTHKD cppEMax
      , ucppNOpt = unTHKD cppNOpt
      , ucppRho = unTHKD cppRho
      , ucppTau = unTHKD cppTau
      , ucppMinPoolCost = fmap mkCanonicalCoin $ unTHKD cppMinPoolCost
      , ucppCoinsPerUTxOByte = fmap (mkCanonicalCoin . unCoinPerByte) $ unTHKD cppCoinsPerUTxOByte
      , ucppCostModels = fmap mkCanonicalCostModels $ unTHKD cppCostModels
      , ucppPrices = fmap mkCanonicalPrices $ unTHKD cppPrices
      , ucppMaxTxExUnits = unTHKD cppMaxTxExUnits <&> unOrdExUnits <&> mkCanonicalExUnits
      , ucppMaxBlockExUnits = unTHKD cppMaxBlockExUnits <&> unOrdExUnits <&> mkCanonicalExUnits
      , ucppMaxValSize = unTHKD cppMaxValSize
      , ucppCollateralPercentage = unTHKD cppCollateralPercentage
      , ucppMaxCollateralInputs = unTHKD cppMaxCollateralInputs
      , ucppPoolVotingThresholds = unTHKD cppPoolVotingThresholds <&> mkCanonicalPoolVotingThresholds
      , ucppDRepVotingThresholds = unTHKD cppDRepVotingThresholds <&> mkCanonicalDRepVotingThresholds
      , ucppCommitteeMinSize = unTHKD cppCommitteeMinSize
      , ucppCommitteeMaxTermLength = unTHKD cppCommitteeMaxTermLength
      , ucppGovActionLifetime = unTHKD cppGovActionLifetime
      , ucppGovActionDeposit = mkCanonicalCoin <$> unTHKD cppGovActionDeposit
      , ucppDRepDeposit = mkCanonicalCoin <$> unTHKD cppDRepDeposit
      , ucppDRepActivity = unTHKD cppDRepActivity
      , ucppMinFeeRefScriptCostPerByte = unTHKD cppMinFeeRefScriptCostPerByte
      }

  fromCanonicalPParamsUpdate CanonicalPParamsUpdate {..} =
    PParamsUpdate
      ConwayPParams
        { cppA0 = THKD ucppA0
        , cppTxFeePerByte = THKD (CoinPerByte <$> fromCanonicalCoin <$> ucppTxFeePerByte)
        , cppTxFeeFixed = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppTxFeeFixed)
        , cppMaxBBSize = THKD ucppMaxBBSize
        , cppMaxTxSize = THKD ucppMaxTxSize
        , cppMaxBHSize = THKD ucppMaxBHSize
        , cppKeyDeposit = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppKeyDeposit)
        , cppPoolDeposit = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppPoolDeposit)
        , cppEMax = THKD ucppEMax
        , cppNOpt = THKD ucppNOpt
        , cppRho = THKD ucppRho
        , cppTau = THKD ucppTau
        , cppProtocolVersion = NoUpdate
        , cppMinPoolCost = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppMinPoolCost)
        , cppCoinsPerUTxOByte = THKD (CoinPerByte <$> fromCanonicalCoin <$> ucppCoinsPerUTxOByte)
        , cppCostModels = THKD (fromCanonicalCostModels <$> ucppCostModels)
        , cppPrices = THKD (fromCanonicalPrices <$> ucppPrices)
        , cppMaxTxExUnits = THKD (ucppMaxTxExUnits <&> fromCanonicalExUnits <&> OrdExUnits)
        , cppMaxBlockExUnits = THKD (ucppMaxBlockExUnits <&> fromCanonicalExUnits <&> OrdExUnits)
        , cppMaxValSize = THKD ucppMaxValSize
        , cppCollateralPercentage = THKD ucppCollateralPercentage
        , cppMaxCollateralInputs = THKD ucppMaxCollateralInputs
        , cppPoolVotingThresholds = THKD (fromCanonicalPoolVotingThresholds <$> ucppPoolVotingThresholds)
        , cppDRepVotingThresholds = THKD (fromCanonicalDRepVotingThresholds <$> ucppDRepVotingThresholds)
        , cppCommitteeMinSize = THKD ucppCommitteeMinSize
        , cppCommitteeMaxTermLength = THKD ucppCommitteeMaxTermLength
        , cppGovActionLifetime = THKD ucppGovActionLifetime
        , cppGovActionDeposit = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppGovActionDeposit)
        , cppDRepDeposit = THKD (fromCanonicalCoin @(CompactForm Coin) <$> ucppDRepDeposit)
        , cppDRepActivity = THKD ucppDRepActivity
        , cppMinFeeRefScriptCostPerByte = THKD ucppMinFeeRefScriptCostPerByte
        }

instance IsCanonicalGovAction (GovAction ConwayEra) where
  mkCanonicalGovAction (ParameterChange purposeId pparamsUpdate mScriptHash) =
    CanonicalParameterChange
      (mkCanonicalPurposeId <$> purposeId)
      (mkCanonicalPParamsUpdate pparamsUpdate)
      mScriptHash
  mkCanonicalGovAction (HardForkInitiation purposeId v) =
    CanonicalHardForkInitiation
      (mkCanonicalPurposeId <$> purposeId)
      v
  mkCanonicalGovAction (TreasuryWithdrawals withdrawals script) =
    CanonicalTreasuryWithdrawals
      (Map.fromList [(mkCanonicalRewardAccount c, mkCanonicalCoin v) | (c, v) <- Map.toList withdrawals])
      script
  mkCanonicalGovAction (NoConfidence purposeId) =
    CanonicalNoConfidence
      (mkCanonicalPurposeId <$> purposeId)
  mkCanonicalGovAction (UpdateCommittee purposeId removedMembers addedMembers newThreshold) =
    CanonicalUpdateCommittee
      (mkCanonicalPurposeId <$> purposeId)
      (Set.fromList [mkCanonicalCredential c | c <- Set.toList removedMembers])
      (Map.fromList [(mkCanonicalCredential c, v) | (c, v) <- Map.toList addedMembers])
      newThreshold
  mkCanonicalGovAction (NewConstitution purposeId constitution) =
    CanonicalNewConstitution
      (mkCanonicalPurposeId <$> purposeId)
      (mkCanonicalConstitution constitution)
  mkCanonicalGovAction (InfoAction) = CanonicalInfoAction

  fromCanonicalGovAction (CanonicalParameterChange purposeId pparamsUpdate mScriptHash) =
    ParameterChange
      (fromCanonicalPurposeId <$> purposeId)
      (fromCanonicalPParamsUpdate pparamsUpdate)
      mScriptHash
  fromCanonicalGovAction (CanonicalHardForkInitiation purposeId v) =
    HardForkInitiation
      (fromCanonicalPurposeId <$> purposeId)
      v
  fromCanonicalGovAction (CanonicalTreasuryWithdrawals withdrawals script) =
    TreasuryWithdrawals
      ( Map.fromList
          [(fromCanonicalRewardAccount c, fromCanonicalCoin v) | (c, v) <- Map.toList withdrawals]
      )
      script
  fromCanonicalGovAction (CanonicalNoConfidence purposeId) =
    NoConfidence
      (fromCanonicalPurposeId <$> purposeId)
  fromCanonicalGovAction (CanonicalUpdateCommittee purposeId removedMembers addedMembers newThreshold) =
    UpdateCommittee
      (fromCanonicalPurposeId <$> purposeId)
      (Set.fromList [fromCanonicalCredential c | c <- Set.toList removedMembers])
      (Map.fromList [(fromCanonicalCredential c, v) | (c, v) <- Map.toList addedMembers])
      newThreshold
  fromCanonicalGovAction (CanonicalNewConstitution purposeId constitution) =
    NewConstitution
      (fromCanonicalPurposeId <$> purposeId)
      (fromCanonicalConstitution constitution)
  fromCanonicalGovAction (CanonicalInfoAction) = InfoAction

instance MkCanonicalGovActionState (GovActionState ConwayEra) where
  mkCanonicalGovActionState GovActionState {..} =
    CanonicalGovActionState
      { gasProposalProcedure = mkCanonicalProposalProcedure gasProposalProcedure
      , gasCommitteeVotes =
          Map.fromList [(mkCanonicalCredential k, mkCanonicalVote v) | (k, v) <- Map.toList gasCommitteeVotes]
      , gasDRepVotes =
          Map.fromList [(mkCanonicalCredential k, mkCanonicalVote v) | (k, v) <- Map.toList gasDRepVotes]
      , gasStakePoolVotes = Map.fromList [(k, mkCanonicalVote v) | (k, v) <- Map.toList gasStakePoolVotes]
      , ..
      }

instance FromCanonicalGovActionState (GovActionState ConwayEra) where
  type ExtraCanonicalGovActionState (GovActionState ConwayEra) = GovActionId
  fromCanonicalGovActionState gasId CanonicalGovActionState {..} =
    GovActionState
      { gasProposalProcedure = fromCanonicalProposalProcedure gasProposalProcedure
      , gasCommitteeVotes =
          Map.fromList
            [(fromCanonicalCredential k, fromCanonicalVote v) | (k, v) <- Map.toList gasCommitteeVotes]
      , gasDRepVotes =
          Map.fromList [(fromCanonicalCredential k, fromCanonicalVote v) | (k, v) <- Map.toList gasDRepVotes]
      , gasStakePoolVotes = Map.fromList [(k, fromCanonicalVote v) | (k, v) <- Map.toList gasStakePoolVotes]
      , ..
      }

instance IsCanonicalVote Vote where
  mkCanonicalVote VoteNo = CanonicalVoteNo
  mkCanonicalVote VoteYes = CanonicalVoteYes
  mkCanonicalVote Abstain = CanonicalAbstain

  fromCanonicalVote CanonicalVoteNo = VoteNo
  fromCanonicalVote CanonicalVoteYes = VoteYes
  fromCanonicalVote CanonicalAbstain = Abstain
