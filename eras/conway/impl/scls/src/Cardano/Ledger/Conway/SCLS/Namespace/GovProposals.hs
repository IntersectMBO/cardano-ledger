{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.SCLS.Namespace.GovProposals (
  GovProposalIn (..),
  CanonicalGovActionState (..),
  mkCanonicalGovActionState,
  CanonicalGovAction,
  mkCanonicalGovAction,
  fromCanonicalGovAction,
  CanonicalProposalProcedure,
  fromCanonicalProposalProcedure,
  mkCanonicalProposalProcedure,
  CanonicalPParamsUpdate (..),
  mkCanonicalPParamsUpdate,
  fromCanonicalPParamsUpdate,
  CanonicalPurposeId (..),
  mkCanonicalPurposeId,
  CanonicalGovActionId (..),
) where

import Cardano.Ledger.Alonzo.PParams (OrdExUnits (..))
import Cardano.Ledger.BaseTypes (
  Anchor (..),
  EpochInterval,
  EpochNo (..),
  NonNegativeInterval,
  ProtVer,
  StrictMaybe,
  UnitInterval,
 )
import Cardano.Ledger.Coin (Coin (..), CoinPerByte (..), CompactForm)
import Cardano.Ledger.Conway (ConwayEra)
import Cardano.Ledger.Conway.Governance (
  Constitution (..),
  GovAction (..),
  GovActionId (..),
  GovActionIx (..),
  GovActionPurpose (..),
  GovActionState (..),
  GovPurposeId (..),
  ProposalProcedure (..),
  Vote (..),
 )
import Cardano.Ledger.Conway.PParams (
  ConwayPParams (..),
  THKD (..),
 )
import Cardano.Ledger.Conway.SCLS.Common (
  CanonicalCredential (..),
  CanonicalRewardAccount (..),
  fromCanonicalCredential,
  fromCanonicalRewardAccount,
  mkCanonicalCredential,
  mkCanonicalRewardAccount,
 )
import Cardano.Ledger.Conway.SCLS.LedgerCBOR (LedgerCBOR (..))
import Cardano.Ledger.Conway.SCLS.Namespace.GovConstitution (
  fromCanonicalConstitution,
  mkCanonicalConstitution,
 )
import Cardano.Ledger.Conway.SCLS.Namespace.GovPParams
import Cardano.Ledger.Conway.SCLS.Namespace.Snapshots ()
import Cardano.Ledger.Core (PParamsUpdate (..))
import Cardano.Ledger.HKD (NoUpdate (..))
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.TxIn
import Cardano.SCLS.CBOR.Canonical
import Cardano.SCLS.CBOR.Canonical.Decoder (
  FromCanonicalCBOR (..),
  decodeListLenCanonical,
  decodeMapLenCanonicalOf,
  decodeWord8Canonical,
 )
import Cardano.SCLS.CBOR.Canonical.Encoder (ToCanonicalCBOR (..), encodeAsMap, mkEncodablePair)
import Cardano.SCLS.Entry.IsKey
import Cardano.SCLS.NamespaceCodec (
  CanonicalCBOREntryDecoder (..),
  CanonicalCBOREntryEncoder (..),
  KnownNamespace (..),
  NamespaceKeySize,
  namespaceKeySize,
 )
import Cardano.SCLS.Versioned (Versioned (..))
import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.MemPack
import Data.MemPack.ByteOrdered
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)

data GovProposalIn = GovProposalIn GovActionId
  deriving (Eq, Ord, Show)

instance MemPack GovActionIx where
  packedByteCount _ = 2
  packM (GovActionIx g) = do
    packWord16beM g
  unpackM = do
    g <- unpackBigEndianM
    return (GovActionIx g)

instance IsKey GovProposalIn where
  keySize = namespaceKeySize @"gov/proposals/v0"
  packKeyM (GovProposalIn GovActionId {..}) = do
    packM gaidTxId
    packM gaidGovActionIx
  unpackKeyM = do
    gaidTxId <- unpackM
    gaidGovActionIx <- unpackM
    return $ GovProposalIn GovActionId {..}

instance ToCanonicalCBOR v CanonicalGovActionState where
  toCanonicalCBOR v CanonicalGovActionState {..} =
    encodeAsMap
      [ mkEncodablePair v ("drep_votes" :: Text) gasDRepVotes
      , mkEncodablePair v ("proposed_in" :: Text) gasProposedIn
      , mkEncodablePair v ("expires_after" :: Text) gasExpiresAfter
      , mkEncodablePair v ("committee_votes" :: Text) gasCommitteeVotes
      , mkEncodablePair v ("stake_pool_votes" :: Text) gasStakePoolVotes
      , mkEncodablePair v ("proposal_procedure" :: Text) gasProposalProcedure
      ]

decodeField :: forall s v a. FromCanonicalCBOR v a => T.Text -> CanonicalDecoder s (Versioned v a)
decodeField fieldName = do
  Versioned s <- fromCanonicalCBOR
  unless (s == fieldName) $
    fail $
      T.unpack $
        "Expected field name " <> fieldName <> " but got " <> s
  fromCanonicalCBOR

mkCanonicalGovActionState :: GovActionState ConwayEra -> CanonicalGovActionState
mkCanonicalGovActionState GovActionState {..} =
  CanonicalGovActionState
    { gasProposalProcedure = mkCanonicalProposalProcedure gasProposalProcedure
    , gasCommitteeVotes =
        Map.fromList [(mkCanonicalCredential k, v) | (k, v) <- Map.toList gasCommitteeVotes]
    , gasDRepVotes = Map.fromList [(mkCanonicalCredential k, v) | (k, v) <- Map.toList gasDRepVotes]
    , ..
    }

data CanonicalGovActionState = CanonicalGovActionState
  { gasCommitteeVotes :: !(Map (CanonicalCredential HotCommitteeRole) Vote)
  , gasDRepVotes :: !(Map (CanonicalCredential DRepRole) Vote)
  , gasStakePoolVotes :: !(Map (KeyHash StakePool) Vote)
  , gasProposalProcedure :: !CanonicalProposalProcedure
  , gasProposedIn :: !EpochNo
  , gasExpiresAfter :: !EpochNo
  }
  deriving (Eq, Show)
  deriving (Generic)

instance FromCanonicalCBOR v CanonicalGovActionState where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 6
    Versioned gasDRepVotes <- decodeField "drep_votes"
    Versioned gasProposedIn <- decodeField "proposed_in"
    Versioned gasExpiresAfter <- decodeField "expires_after"
    Versioned gasCommitteeVotes <- decodeField "committee_votes"
    Versioned gasStakePoolVotes <- decodeField "stake_pool_votes"
    Versioned gasProposalProcedure <- decodeField "proposal_procedure"
    pure $ Versioned CanonicalGovActionState {..}

data CanonicalGovAction
  = CanonicalParameterChange
      (StrictMaybe (CanonicalPurposeId PParamUpdatePurpose))
      CanonicalPParamsUpdate
      (StrictMaybe ScriptHash)
  | CanonicalHardForkInitiation (StrictMaybe (CanonicalPurposeId HardForkPurpose)) ProtVer
  | CanonicalTreasuryWithdrawals (Map CanonicalRewardAccount Coin) (StrictMaybe ScriptHash)
  | CanonicalNoConfidence (StrictMaybe (CanonicalPurposeId CommitteePurpose))
  | CanonicalUpdateCommittee
      (StrictMaybe (CanonicalPurposeId CommitteePurpose))
      (Set (CanonicalCredential ColdCommitteeRole))
      (Map (CanonicalCredential ColdCommitteeRole) EpochNo)
      UnitInterval
  | CanonicalNewConstitution
      (StrictMaybe (CanonicalPurposeId ConstitutionPurpose))
      (Constitution ConwayEra)
  | CanonicalInfoAction
  deriving (Eq, Show, Generic)

mkCanonicalGovAction :: GovAction ConwayEra -> CanonicalGovAction
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
    (Map.fromList [(mkCanonicalRewardAccount c, v) | (c, v) <- Map.toList withdrawals])
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
    constitution
mkCanonicalGovAction (InfoAction) = CanonicalInfoAction

fromCanonicalGovAction :: CanonicalGovAction -> GovAction ConwayEra
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
    (Map.fromList [(fromCanonicalRewardAccount c, v) | (c, v) <- Map.toList withdrawals])
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
    constitution
fromCanonicalGovAction (CanonicalInfoAction) = InfoAction

data CanonicalProposalProcedure = CanonicalProposalProcedure
  { pProcAnchor :: Anchor
  , pProcDeposit :: Coin
  , pProcGovAction :: CanonicalGovAction
  , pProcReturnAddr :: CanonicalRewardAccount
  }
  deriving (Eq, Show, Generic)

mkCanonicalProposalProcedure :: ProposalProcedure ConwayEra -> CanonicalProposalProcedure
mkCanonicalProposalProcedure ProposalProcedure {..} =
  CanonicalProposalProcedure
    { pProcGovAction = mkCanonicalGovAction pProcGovAction
    , pProcReturnAddr = mkCanonicalRewardAccount pProcReturnAddr
    , ..
    }

fromCanonicalProposalProcedure :: CanonicalProposalProcedure -> ProposalProcedure ConwayEra
fromCanonicalProposalProcedure CanonicalProposalProcedure {..} =
  ProposalProcedure
    { pProcGovAction = fromCanonicalGovAction pProcGovAction
    , pProcReturnAddr = fromCanonicalRewardAccount pProcReturnAddr
    , ..
    }

instance ToCanonicalCBOR v CanonicalProposalProcedure where
  toCanonicalCBOR v CanonicalProposalProcedure {..} =
    encodeAsMap
      [ mkEncodablePair v ("anchor" :: Text) pProcAnchor
      , mkEncodablePair v ("deposit" :: Text) pProcDeposit
      , mkEncodablePair v ("gov_action" :: Text) pProcGovAction
      , mkEncodablePair v ("return_address" :: Text) pProcReturnAddr
      ]

instance FromCanonicalCBOR v CanonicalProposalProcedure where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 4
    Versioned pProcAnchor <- decodeField "anchor"
    Versioned pProcDeposit <- decodeField "deposit"
    Versioned pProcGovAction <- decodeField "gov_action"
    Versioned pProcReturnAddr <- decodeField "return_address"
    pure $ Versioned CanonicalProposalProcedure {..}

instance ToCanonicalCBOR v CanonicalGovAction where
  toCanonicalCBOR v (CanonicalParameterChange purposeId pparamsUpdate mScriptHash) =
    toCanonicalCBOR v (0 :: Word8, purposeId, pparamsUpdate, mScriptHash)
  toCanonicalCBOR v (CanonicalHardForkInitiation purposeId protVer) =
    toCanonicalCBOR v (1 :: Word8, purposeId, protVer)
  toCanonicalCBOR v (CanonicalTreasuryWithdrawals withdrawals mScriptHash) =
    toCanonicalCBOR v (2 :: Word8, withdrawals, mScriptHash)
  toCanonicalCBOR v (CanonicalNoConfidence purposeId) =
    toCanonicalCBOR v (3 :: Word8, purposeId)
  toCanonicalCBOR v (CanonicalUpdateCommittee purposeId removedMembers addedMembers newThreshold) =
    toCanonicalCBOR v (4 :: Word8, purposeId, removedMembers, addedMembers, newThreshold)
  toCanonicalCBOR v (CanonicalNewConstitution purposeId constitution) =
    let canonicalConstitution = mkCanonicalConstitution constitution
     in toCanonicalCBOR v (5 :: Word8, purposeId, canonicalConstitution)
  toCanonicalCBOR v (CanonicalInfoAction) =
    toCanonicalCBOR v (6 :: Word8, ())

instance FromCanonicalCBOR v CanonicalGovAction where
  fromCanonicalCBOR = do
    l <- decodeListLenCanonical
    tag <- decodeWord8Canonical
    case tag of
      0 | l == 4 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned pparamsUpdate <- fromCanonicalCBOR
        Versioned mScriptHash <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalParameterChange purposeId pparamsUpdate mScriptHash
      1 | l == 3 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned protVer <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalHardForkInitiation purposeId protVer
      2 | l == 3 -> do
        Versioned withdrawals <- fromCanonicalCBOR
        Versioned mScriptHash <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalTreasuryWithdrawals withdrawals mScriptHash
      3 | l == 2 -> do
        Versioned purposeId <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalNoConfidence purposeId
      4 | l == 5 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned removedMembers <- fromCanonicalCBOR
        Versioned addedMembers <- fromCanonicalCBOR
        Versioned newThreshold <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalUpdateCommittee purposeId removedMembers addedMembers newThreshold
      5 | l == 3 -> do
        Versioned purposeId <- fromCanonicalCBOR
        Versioned (fromCanonicalConstitution -> constitution) <- fromCanonicalCBOR
        pure $ Versioned $ CanonicalNewConstitution purposeId constitution
      6 | l == 2 -> do
        Versioned () <- fromCanonicalCBOR
        pure $ Versioned CanonicalInfoAction
      _ -> fail $ "Unknown GovAction tag: " ++ show tag

deriving via LedgerCBOR v Vote instance ToCanonicalCBOR v Vote

deriving via LedgerCBOR v Vote instance FromCanonicalCBOR v Vote

type instance NamespaceKeySize "gov/proposals/v0" = 34

instance KnownNamespace "gov/proposals/v0" where
  type NamespaceKey "gov/proposals/v0" = GovProposalIn
  type NamespaceEntry "gov/proposals/v0" = CanonicalGovActionState

instance CanonicalCBOREntryEncoder "gov/proposals/v0" CanonicalGovActionState where
  encodeEntry n = toCanonicalCBOR (Proxy @"gov/proposals/v0") n

instance CanonicalCBOREntryDecoder "gov/proposals/v0" CanonicalGovActionState where
  decodeEntry = fromCanonicalCBOR

data CanonicalPParamsUpdate = CanonicalPParamsUpdate
  { ucppA0 :: StrictMaybe NonNegativeInterval
  , ucppTxFeePerByte :: StrictMaybe (CompactForm Coin)
  , ucppTxFeeFixed :: StrictMaybe (CompactForm Coin)
  , ucppMaxBBSize :: StrictMaybe Word32
  , ucppMaxTxSize :: StrictMaybe Word32
  , ucppMaxBHSize :: StrictMaybe Word16
  , ucppKeyDeposit :: StrictMaybe (CompactForm Coin)
  , ucppPoolDeposit :: StrictMaybe (CompactForm Coin)
  , ucppEMax :: StrictMaybe EpochInterval
  , ucppNOpt :: StrictMaybe Word16
  , ucppRho :: StrictMaybe UnitInterval
  , ucppTau :: StrictMaybe UnitInterval
  , ucppMinPoolCost :: StrictMaybe (CompactForm Coin)
  , ucppCoinsPerUTxOByte :: StrictMaybe (CompactForm Coin)
  , ucppCostModels :: StrictMaybe CanonicalCostModels
  , ucppPrices :: StrictMaybe CanonicalPrices
  , ucppMaxTxExUnits :: StrictMaybe CanonicalExUnits
  , ucppMaxBlockExUnits :: StrictMaybe CanonicalExUnits
  , ucppMaxValSize :: StrictMaybe Word32
  , ucppCollateralPercentage :: StrictMaybe Word16
  , ucppMaxCollateralInputs :: StrictMaybe Word16
  , ucppPoolVotingThresholds :: StrictMaybe CanonicalPoolVotingThresholds
  , ucppDRepVotingThresholds :: StrictMaybe CanonicalDRepVotingThresholds
  , ucppCommitteeMinSize :: StrictMaybe Word16
  , ucppCommitteeMaxTermLength :: StrictMaybe EpochInterval
  , ucppGovActionLifetime :: StrictMaybe EpochInterval
  , ucppGovActionDeposit :: StrictMaybe (CompactForm Coin)
  , ucppDRepDeposit :: StrictMaybe (CompactForm Coin)
  , ucppDRepActivity :: StrictMaybe EpochInterval
  , ucppMinFeeRefScriptCostPerByte :: StrictMaybe NonNegativeInterval
  }
  deriving (Eq, Show, Generic)

mkCanonicalPParamsUpdate :: PParamsUpdate ConwayEra -> CanonicalPParamsUpdate
mkCanonicalPParamsUpdate (PParamsUpdate ConwayPParams {..}) =
  CanonicalPParamsUpdate
    { ucppA0 = unTHKD cppA0
    , ucppTxFeePerByte = fmap unCoinPerByte $ unTHKD cppTxFeePerByte
    , ucppTxFeeFixed = unTHKD cppTxFeeFixed
    , ucppMaxBBSize = unTHKD cppMaxBBSize
    , ucppMaxTxSize = unTHKD cppMaxTxSize
    , ucppMaxBHSize = unTHKD cppMaxBHSize
    , ucppKeyDeposit = unTHKD cppKeyDeposit
    , ucppPoolDeposit = unTHKD cppPoolDeposit
    , ucppEMax = unTHKD cppEMax
    , ucppNOpt = unTHKD cppNOpt
    , ucppRho = unTHKD cppRho
    , ucppTau = unTHKD cppTau
    , ucppMinPoolCost = unTHKD cppMinPoolCost
    , ucppCoinsPerUTxOByte = fmap unCoinPerByte $ unTHKD cppCoinsPerUTxOByte
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
    , ucppGovActionDeposit = unTHKD cppGovActionDeposit
    , ucppDRepDeposit = unTHKD cppDRepDeposit
    , ucppDRepActivity = unTHKD cppDRepActivity
    , ucppMinFeeRefScriptCostPerByte = unTHKD cppMinFeeRefScriptCostPerByte
    }

fromCanonicalPParamsUpdate :: CanonicalPParamsUpdate -> PParamsUpdate ConwayEra
fromCanonicalPParamsUpdate CanonicalPParamsUpdate {..} =
  PParamsUpdate
    ConwayPParams
      { cppA0 = THKD ucppA0
      , cppTxFeePerByte = THKD (CoinPerByte <$> ucppTxFeePerByte)
      , cppTxFeeFixed = THKD ucppTxFeeFixed
      , cppMaxBBSize = THKD ucppMaxBBSize
      , cppMaxTxSize = THKD ucppMaxTxSize
      , cppMaxBHSize = THKD ucppMaxBHSize
      , cppKeyDeposit = THKD ucppKeyDeposit
      , cppPoolDeposit = THKD ucppPoolDeposit
      , cppEMax = THKD ucppEMax
      , cppNOpt = THKD ucppNOpt
      , cppRho = THKD ucppRho
      , cppTau = THKD ucppTau
      , cppProtocolVersion = NoUpdate
      , cppMinPoolCost = THKD ucppMinPoolCost
      , cppCoinsPerUTxOByte = THKD (CoinPerByte <$> ucppCoinsPerUTxOByte)
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
      , cppGovActionDeposit = THKD ucppGovActionDeposit
      , cppDRepDeposit = THKD ucppDRepDeposit
      , cppDRepActivity = THKD ucppDRepActivity
      , cppMinFeeRefScriptCostPerByte = THKD ucppMinFeeRefScriptCostPerByte
      }

instance ToCanonicalCBOR v CanonicalPParamsUpdate where
  toCanonicalCBOR v CanonicalPParamsUpdate {..} =
    encodeAsMap
      [ mkEncodablePair v ("a0" :: Text) ucppA0
      , mkEncodablePair v ("rho" :: Text) ucppRho
      , mkEncodablePair v ("tau" :: Text) ucppTau
      , mkEncodablePair v ("n_opt" :: Text) ucppNOpt
      , mkEncodablePair v ("prices" :: Text) ucppPrices
      , mkEncodablePair v ("epoch_max" :: Text) ucppEMax
      , mkEncodablePair v ("min_fee_a" :: Text) ucppTxFeePerByte
      , mkEncodablePair v ("min_fee_b" :: Text) ucppTxFeeFixed
      , mkEncodablePair v ("cost_models" :: Text) ucppCostModels
      , mkEncodablePair v ("key_deposit" :: Text) ucppKeyDeposit
      , mkEncodablePair v ("max_tx_size" :: Text) ucppMaxTxSize
      , mkEncodablePair v ("drep_deposit" :: Text) ucppDRepDeposit
      , mkEncodablePair v ("pool_deposit" :: Text) ucppPoolDeposit
      , mkEncodablePair v ("max_val_size" :: Text) ucppMaxValSize
      , mkEncodablePair v ("drep_activity" :: Text) ucppDRepActivity
      , mkEncodablePair v ("min_pool_cost" :: Text) ucppMinPoolCost
      , mkEncodablePair v ("max_block_size" :: Text) ucppMaxBBSize
      , mkEncodablePair v ("max_tx_ex_units" :: Text) ucppMaxTxExUnits
      , mkEncodablePair v ("coin_per_utxo_byte" :: Text) ucppCoinsPerUTxOByte
      , mkEncodablePair v ("gov_action_deposit" :: Text) ucppGovActionDeposit
      , mkEncodablePair v ("max_block_ex_units" :: Text) ucppMaxBlockExUnits
      , mkEncodablePair v ("min_committee_size" :: Text) ucppCommitteeMinSize
      , mkEncodablePair v ("committee_term_limit" :: Text) ucppCommitteeMaxTermLength
      , mkEncodablePair v ("collateral_percentage" :: Text) ucppCollateralPercentage
      , mkEncodablePair v ("drep_voting_thresholds" :: Text) ucppDRepVotingThresholds
      , mkEncodablePair v ("gov_action_lifetime" :: Text) ucppGovActionLifetime
      , mkEncodablePair v ("max_block_header_size" :: Text) ucppMaxBHSize
      , mkEncodablePair v ("max_collateral_inputs" :: Text) ucppMaxCollateralInputs
      , mkEncodablePair v ("pool_voting_thresholds" :: Text) ucppPoolVotingThresholds
      , mkEncodablePair
          v
          ("min_fee_ref_script_cost_per_byte" :: Text)
          (ucppMinFeeRefScriptCostPerByte)
      ]

instance FromCanonicalCBOR v CanonicalPParamsUpdate where
  fromCanonicalCBOR = do
    decodeMapLenCanonicalOf 30
    Versioned ucppA0 <- decodeField "a0"
    Versioned ucppRho <- decodeField "rho"
    Versioned ucppTau <- decodeField "tau"
    Versioned ucppNOpt <- decodeField "n_opt"
    Versioned ucppPrices <- decodeField "prices"
    Versioned ucppEMax <- decodeField "epoch_max"
    Versioned ucppTxFeePerByte <- decodeField "min_fee_a"
    Versioned ucppTxFeeFixed <- decodeField "min_fee_b"
    Versioned ucppCostModels <- decodeField "cost_models"
    Versioned ucppKeyDeposit <- decodeField "key_deposit"
    Versioned ucppMaxTxSize <- decodeField "max_tx_size"
    Versioned ucppDRepDeposit <- decodeField "drep_deposit"
    Versioned ucppMaxValSize <- decodeField "max_val_size"
    Versioned ucppPoolDeposit <- decodeField "pool_deposit"
    Versioned ucppDRepActivity <- decodeField "drep_activity"
    Versioned ucppMinPoolCost <- decodeField "min_pool_cost"
    Versioned ucppMaxBBSize <- decodeField "max_block_size"
    Versioned ucppMaxTxExUnits <- decodeField "max_tx_ex_units"
    Versioned ucppCoinsPerUTxOByte <- decodeField "coin_per_utxo_byte"
    Versioned ucppGovActionDeposit <- decodeField "gov_action_deposit"
    Versioned ucppMaxBlockExUnits <- decodeField "max_block_ex_units"
    Versioned ucppCommitteeMinSize <- decodeField "min_committee_size"
    Versioned ucppGovActionLifetime <- decodeField "gov_action_lifetime"
    Versioned ucppCommitteeMaxTermLength <- decodeField "committee_term_limit"
    Versioned ucppCollateralPercentage <- decodeField "collateral_percentage"
    Versioned ucppMaxBHSize <- decodeField "max_block_header_size"
    Versioned ucppMaxCollateralInputs <- decodeField "max_collateral_inputs"
    Versioned ucppDRepVotingThresholds <- decodeField "drep_voting_thresholds"
    Versioned ucppPoolVotingThresholds <- decodeField "pool_voting_thresholds"
    Versioned ucppMinFeeRefScriptCostPerByte <- decodeField "min_fee_ref_script_cost_per_byte"
    pure $ Versioned CanonicalPParamsUpdate {..}

newtype CanonicalPurposeId (p :: GovActionPurpose) = CanonicalPurposeId CanonicalGovActionId
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v (CanonicalPurposeId p) where
  toCanonicalCBOR v (CanonicalPurposeId p) = toCanonicalCBOR v p

instance FromCanonicalCBOR v (CanonicalPurposeId p) where
  fromCanonicalCBOR = fmap CanonicalPurposeId <$> fromCanonicalCBOR

mkCanonicalPurposeId :: GovPurposeId p -> CanonicalPurposeId p
mkCanonicalPurposeId (GovPurposeId p) = CanonicalPurposeId (mkCanonicalGovActionId p)

fromCanonicalPurposeId :: CanonicalPurposeId p -> GovPurposeId p
fromCanonicalPurposeId (CanonicalPurposeId p) = GovPurposeId (fromCanonicalGovActionId p)

data CanonicalGovActionId = CanonicalGovActionId
  { gaidTxId :: TxId
  , gaidGovActionIx :: {-# UNPACK #-} !GovActionIx
  }
  deriving (Eq, Show, Generic)

instance ToCanonicalCBOR v CanonicalGovActionId where
  toCanonicalCBOR v CanonicalGovActionId {..} = toCanonicalCBOR v (gaidTxId, gaidGovActionIx)

instance FromCanonicalCBOR v CanonicalGovActionId where
  fromCanonicalCBOR = do
    Versioned (gaidTxId, gaidGovActionIx) <- fromCanonicalCBOR
    return $ Versioned CanonicalGovActionId {..}

mkCanonicalGovActionId :: GovActionId -> CanonicalGovActionId
mkCanonicalGovActionId GovActionId {..} = CanonicalGovActionId {..}

fromCanonicalGovActionId :: CanonicalGovActionId -> GovActionId
fromCanonicalGovActionId CanonicalGovActionId {..} = GovActionId {..}

deriving via LedgerCBOR v TxId instance ToCanonicalCBOR v TxId

deriving via LedgerCBOR v TxId instance FromCanonicalCBOR v TxId

deriving via LedgerCBOR v GovActionIx instance ToCanonicalCBOR v GovActionIx

deriving via LedgerCBOR v GovActionIx instance FromCanonicalCBOR v GovActionIx
