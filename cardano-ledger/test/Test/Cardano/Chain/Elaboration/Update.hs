{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Elaboration.Update
  ( elaboratePParams
  , elaborateProtocolVersion
  , elaborateSoftwareVersion
  , elaborateUpdateProposal
  , elaborateVote
  )
where

import Cardano.Prelude

import Control.Arrow ((|||))
import Data.Coerce (coerce)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Cardano.Crypto (ProtocolMagicId)
import qualified Cardano.Crypto.Hashing as H

import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Slotting as Concrete
import qualified Cardano.Chain.Update as Concrete
import qualified Cardano.Chain.Update.Proposal as Proposal

import Ledger.Core (unSlotCount)
import qualified Ledger.Update as Abstract


import Test.Cardano.Chain.Elaboration.Keys (vKeyToSafeSigner)
import Test.Cardano.Chain.Genesis.Dummy (dummyProtocolParameters)

elaboratePParams :: Abstract.PParams -> Concrete.ProtocolParameters
elaboratePParams pps = Concrete.ProtocolParameters
  { Concrete.ppScriptVersion      = fromIntegral $ Abstract._scriptVersion pps
  , Concrete.ppSlotDuration       = 0 -- TODO: was Concrete.ppSlotDuration dummyProtocolParameters
  , Concrete.ppMaxBlockSize       = 748 * Abstract._maxBkSz pps
  , Concrete.ppMaxHeaderSize      = 95 * Abstract._maxHdrSz pps
  , Concrete.ppMaxTxSize          = 4096 * Abstract._maxTxSz pps
  , Concrete.ppMaxProposalSize    = 4096 * Abstract._maxPropSz pps
  , Concrete.ppMpcThd             = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppHeavyDelThd        = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateVoteThd      = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateProposalThd  = Concrete.mkKnownLovelacePortion @0
  , Concrete.ppUpdateProposalTTL  = Concrete.SlotNumber
                                  $ unSlotCount
                                  $ Abstract._upTtl pps
  , Concrete.ppSoftforkRule       =
    Concrete.SoftforkRule
      { Concrete.srInitThd = Concrete.mkKnownLovelacePortion @0
      -- See 'upAdptThd' in 'module Cardano.Chain.Update.ProtocolParameters'
      , Concrete.srMinThd = panic . show ||| identity
                          $ Concrete.mkLovelacePortion
                          $ floor
                          $ Abstract._upAdptThd pps * 1e15
      , Concrete.srThdDecrement  = Concrete.mkKnownLovelacePortion @0
      }
  , Concrete.ppTxFeePolicy        = Concrete.TxFeePolicyTxSizeLinear
    (Concrete.TxSizeLinear
      (intToLovelace 0)
      (intToLovelace 0)
      -- (intToLovelace (Abstract._factorA pps))
      -- (intToLovelace (Abstract._factorB pps))
    )
  , Concrete.ppUnlockStakeEpoch   = Concrete.EpochNumber maxBound
  }
 where
  intToLovelace :: Int -> Concrete.Lovelace
  intToLovelace x =
    case Concrete.mkLovelace (fromIntegral x) of
    Left err -> panic $ "intToLovelace: " <> show err
    Right l -> l

elaborateProtocolVersion
  :: Abstract.ProtVer
  -> Concrete.ProtocolVersion
elaborateProtocolVersion (Abstract.ProtVer major minor alternative) =
  -- TODO: the abstract version numbers should have the same type as the
  -- concrete ones!
  Concrete.ProtocolVersion
    (fromIntegral major)
    (fromIntegral minor)
    (fromIntegral alternative)

elaborateSoftwareVersion
  :: Abstract.SwVer
  -> Concrete.SoftwareVersion
elaborateSoftwareVersion abstractVersion =
  Concrete.SoftwareVersion applicationName' applicationVersion'
  where
    Abstract.SwVer
      (Abstract.ApName applicationName)
      (Abstract.ApVer applicationVersion) = abstractVersion
    applicationName' = Concrete.ApplicationName $ Text.pack applicationName
    applicationVersion' = fromIntegral applicationVersion :: Concrete.NumSoftwareVersion

elaborateUpdateProposal
  :: ProtocolMagicId
  -> Abstract.UProp
  -> Concrete.AProposal ()
elaborateUpdateProposal protocolMagicId abstractProposal =
  Concrete.signProposal
    protocolMagicId
    body
    safeSigner
  where
    body = elaborateProposalBody abstractProposal
    safeSigner = vKeyToSafeSigner $ Abstract._upIssuer abstractProposal

elaborateProposalBody
  :: Abstract.UProp
  -> Concrete.ProposalBody
elaborateProposalBody proposal =
  Proposal.ProposalBody
  { Proposal.protocolVersion =
      elaborateProtocolVersion $ Abstract._upPV proposal
  , Proposal.protocolParametersUpdate =
      justifyProtocolParameters $ elaboratePParams $ Abstract._upParams proposal
  , Proposal.softwareVersion =
      elaborateSoftwareVersion $ Abstract._upSwVer proposal
  , Proposal.metadata =
      Map.fromList $ zip systemTags systemHashes
  }
  where
    systemTags = fmap elaborateSystemTag $ Set.toList $ Abstract._upSTags proposal
    -- TODO: we might need different hashes here, which means that either the
    -- elaborators should be able to generate random data, or the abstract
    -- update payload should include (an abstract version of) these hashes.
    systemHashes = repeat $ Concrete.InstallerHash $ coerce $ H.hash ("" :: ByteString)

-- | Convert a 'ProtocolParameters' value to a 'ProtocolParametersUpdate'
--
justifyProtocolParameters
  :: Concrete.ProtocolParameters
  -> Concrete.ProtocolParametersUpdate
justifyProtocolParameters parameters =
  Concrete.ProtocolParametersUpdate
  { Concrete.ppuScriptVersion = Just $ Concrete.ppScriptVersion parameters
  , Concrete.ppuSlotDuration = Just $ Concrete.ppSlotDuration parameters
  , Concrete.ppuMaxBlockSize = Just $ Concrete.ppMaxBlockSize parameters
  , Concrete.ppuMaxHeaderSize = Just $ Concrete.ppMaxHeaderSize parameters
  , Concrete.ppuMaxTxSize = Just $ Concrete.ppMaxTxSize parameters
  , Concrete.ppuMaxProposalSize = Just $ Concrete.ppMaxProposalSize parameters
  , Concrete.ppuMpcThd = Just $ Concrete.ppMpcThd parameters
  , Concrete.ppuHeavyDelThd = Just $ Concrete.ppHeavyDelThd parameters
  , Concrete.ppuUpdateVoteThd = Just $ Concrete.ppUpdateVoteThd parameters
  , Concrete.ppuUpdateProposalThd = Just $ Concrete.ppUpdateProposalThd parameters
  , Concrete.ppuUpdateProposalTTL = Just $ Concrete.ppUpdateProposalTTL parameters
  , Concrete.ppuSoftforkRule = Just $ Concrete.ppSoftforkRule parameters
  , Concrete.ppuTxFeePolicy = Just $ Concrete.ppTxFeePolicy parameters
  , Concrete.ppuUnlockStakeEpoch = Just $ Concrete.ppUnlockStakeEpoch parameters
  }


elaborateSystemTag :: Abstract.STag -> Concrete.SystemTag
elaborateSystemTag = Concrete.SystemTag . Text.pack

elaborateVote
  :: ProtocolMagicId
  -> Map Abstract.UpId Concrete.UpId
  -> Abstract.Vote
  -> Concrete.AVote ()
elaborateVote protocolMagicId proposalsIdMap abstractVote =
  Concrete.mkVoteSafe
    protocolMagicId
    safeSigner
    proposalId
    True
  where
    safeSigner = vKeyToSafeSigner $ Abstract._vCaster abstractVote
    proposalId =
      fromMaybe
        error
        (Map.lookup abstractProposalId proposalsIdMap)
      where
        abstractProposalId = Abstract._vPropId abstractVote
        error :: Concrete.UpId -- Keeps GHC happy ...
        error = panic $ "Could not find a concrete proposal id"
                      <> " that corresponds to an abstract proposal id:"
                      <> show abstractProposalId
