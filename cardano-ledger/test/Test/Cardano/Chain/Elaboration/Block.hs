{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

-- | This module provides functionality for translating abstract blocks into
-- concrete blocks. The abstract blocks are generated according the small-step
-- rules for the block chain (also called the blockchain specification).
module Test.Cardano.Chain.Elaboration.Block
  ( abEnvToCfg
  , elaborate
  , elaborateBS
  , rcDCert
  , AbstractToConcreteIdMaps
    ( AbstractToConcreteIdMaps
    , proposalIds
    , transactionIds
    )
  )
where

import Cardano.Prelude hiding (to)

import Control.Arrow ((&&&))
import Control.Lens ((^.), to, (^..))
import Data.Bimap (Bimap)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid.Generic (GenericSemigroup (GenericSemigroup), GenericMonoid (GenericMonoid))
import qualified Data.Set as Set
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime))
import GHC.Generics (Generic)

import qualified Cardano.Binary as Binary
import qualified Cardano.Crypto.Hashing as H
import Cardano.Crypto.ProtocolMagic (AProtocolMagic(..))

import qualified Cardano.Chain.Block as Concrete
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Ssc as Ssc
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Slotting as Slotting

import qualified Control.State.Transition as Transition
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN, disL)
import qualified Cardano.Spec.Chain.STS.Rule.Epoch as Abstract
import qualified Ledger.Core as Abstract
import Ledger.Delegation
  (DCert, delegationMap, delegatorOf, mkDCert)
import Ledger.Update (maxBkSz, maxHdrSz, maxTxSz)
import qualified Ledger.Update as Abstract.Update
import qualified Ledger.UTxO as Abstract
import Cardano.Chain.Common
  ( BlockCount(BlockCount)
  , ChainDifficulty(ChainDifficulty)
  , LovelacePortion(LovelacePortion)
  , TxFeePolicy(TxFeePolicyTxSizeLinear)
  , TxSizeLinear(TxSizeLinear)
  , mkKnownLovelace
  , hashKey
  )

import Test.Cardano.Chain.Elaboration.Keys
  (elaborateVKeyGenesis, vKeyPair, vKeyToSKey)
import Test.Cardano.Chain.Elaboration.Delegation (elaborateDCert)
import Test.Cardano.Chain.Elaboration.Update
  ( elaborateProtocolVersion
  , elaborateSoftwareVersion
  , elaborateUpdateProposal
  , elaborateVote
  )
import Test.Cardano.Chain.UTxO.Model (elaborateTxWitnesses)
import qualified Test.Cardano.Crypto.Dummy as Dummy


data AbstractToConcreteIdMaps = AbstractToConcreteIdMaps
  { transactionIds :: !(Map Abstract.TxId UTxO.TxId)
  , proposalIds :: !(Map Abstract.Update.UpId Update.UpId)
  } deriving (Eq, Show, Generic)
  deriving Semigroup via GenericSemigroup AbstractToConcreteIdMaps
  deriving Monoid via GenericMonoid AbstractToConcreteIdMaps

-- | Elaborate an abstract block into a concrete block (without annotations).
elaborate
  :: AbstractToConcreteIdMaps
  -> Genesis.Config
  -> DCert
  -> Concrete.ChainValidationState
  -> Abstract.Block
  -> (Concrete.Block, AbstractToConcreteIdMaps)
elaborate abstractToConcreteIdMaps config dCert st abstractBlock =
  ( Concrete.ABlock
    { Concrete.blockHeader     = bh0
    , Concrete.blockBody       = bb0
    , Concrete.blockAnnotation = ()
    }
  , AbstractToConcreteIdMaps
    { transactionIds = txIdMap'
    , proposalIds = proposalsIdMap'
    }
  )
 where
  AbstractToConcreteIdMaps txIdMap proposalsIdMap = abstractToConcreteIdMaps

  pm = Genesis.configProtocolMagicId config

  bh0 = Concrete.mkHeaderExplicit
    pm
    prevHash
    (ChainDifficulty 0)
    (Genesis.configEpochSlots config)
    sid
    ssk
    cDCert
    bb0
    (elaborateProtocolVersion $ Abstract._bProtVer $ Abstract._bBody abstractBlock)
    -- TODO: the Byron spec needs to incorporate a software version in the blocks
    (elaborateSoftwareVersion $ Abstract.Update.SwVer (Abstract.Update.ApName "") (Abstract.Update.ApVer 0))

  prevHash :: Concrete.HeaderHash
  prevHash =
    either Concrete.genesisHeaderHash identity $ Concrete.cvsPreviousHash st

  sid = Slotting.SlotNumber
    (abstractBlock ^. Abstract.bHeader . Abstract.bhSlot . to Abstract.unSlot)

  issuer = abstractBlock ^. Abstract.bHeader . Abstract.bhIssuer

  ssk = vKeyToSKey issuer

  cDCert :: Delegation.Certificate
  cDCert = elaborateDCert pm dCert

  bb0    = Concrete.ABody
    { Concrete.bodyTxPayload     = UTxO.ATxPayload txPayload
    , Concrete.bodySscPayload    = Ssc.SscPayload
    , Concrete.bodyDlgPayload    = Delegation.UnsafeAPayload dcerts ()
    , Concrete.bodyUpdatePayload = updatePayload
    }

  dcerts =
    abstractBlock
      ^.. (Abstract.bBody . Abstract.bDCerts . traverse . to
            (elaborateDCert pm)
          )

  (txPayload, txIdMap') = first (fmap void) $ elaborateTxWitnesses
    txIdMap
    (reverse $ abstractBlock ^. Abstract.bBody . Abstract.bUtxo)

  updatePayload :: Update.APayload ()
  updatePayload =
    Update.APayload
      (fmap snd maybeProposals)
      (fmap (elaborateVote pm proposalsIdMap')
      $ Abstract._bUpdVotes
      $ Abstract._bBody abstractBlock
      )
      () -- Update payload annotation

  maybeProposals :: Maybe (Abstract.Update.UProp, Update.Proposal)
  maybeProposals
    = fmap (identity &&& elaborateUpdateProposal pm)
    $ Abstract._bUpdProp
    $ Abstract._bBody abstractBlock

  proposalsIdMap' :: Map Abstract.Update.UpId Update.UpId
  proposalsIdMap' = maybe proposalsIdMap addUpdateProposalId maybeProposals
    where
      addUpdateProposalId (abstractProposal, concreteProposal) =
        Map.insert
          (Abstract.Update._upId abstractProposal)
          (H.hash concreteProposal)
          proposalsIdMap


elaborateBS
  :: AbstractToConcreteIdMaps
  -> Genesis.Config -- TODO: Do we want this to come from the abstract
                    -- environment? (in such case we wouldn't need this
                    -- parameter)
  -> DCert
  -> Concrete.ChainValidationState
  -> Abstract.Block
  -> (Concrete.ABlock ByteString, AbstractToConcreteIdMaps)
elaborateBS txIdMap config dCert st ab =
  first (annotateBlock (Genesis.configEpochSlots config))
    $ elaborate txIdMap config dCert st ab

annotateBlock :: Slotting.EpochSlots -> Concrete.Block -> Concrete.ABlock ByteString
annotateBlock epochSlots block =
  let
    decodedABlockOrBoundary =
      case
          Binary.decodeFullDecoder
            "Block"
            (Concrete.fromCBORABlockOrBoundary epochSlots) bytes
        of
          Left err ->
            panic
              $  "This function should be able to decode the block it encoded"
              <> ". Instead I got: "
              <> show err
          Right abobb -> map (LBS.toStrict . Binary.slice bytes) abobb
  in
    case decodedABlockOrBoundary of
      Concrete.ABOBBlock bk -> bk
      Concrete.ABOBBoundary _ ->
        panic "This function should have decoded a block."
  where bytes = Binary.serializeEncoding (Concrete.toCBORABOBBlock epochSlots block)

-- | Re-construct an abstract delegation certificate from the abstract state.
--
-- We need to do this because the delegation certificate is included in the
-- block.
rcDCert
  :: Abstract.VKey
  -- ^ Key for which the delegation certificate is being constructed.
  -> Abstract.BlockCount
  -- ^ Chain stability parameter
  -> Transition.State CHAIN
  -> DCert
rcDCert vk k ast@(slot, _, _, _, _, _) =
  mkDCert vkg sigVkgEpoch vk epoch
 where
  dm :: Bimap Abstract.VKeyGenesis Abstract.VKey
  dm = ast ^. disL . delegationMap

  vkg = fromMaybe err $ delegatorOf dm vk

  err :: Abstract.VKeyGenesis
  err = panic $ "No delegator found for key " <> show vk

  vkp = vKeyPair $ coerce vkg

  sigVkgEpoch = Abstract.sign (Abstract.sKey vkp) (vk, epoch)

  epoch = Abstract.sEpoch slot k

-- | Make a genesis configuration from an initial abstract environment of the
--   trace.
--
abEnvToCfg :: Transition.Environment CHAIN -> Genesis.Config
abEnvToCfg (_currentSlot, _genesisUtxo, allowedDelegators, protocolParams, stableAfter) =
  Genesis.Config genesisData genesisHash Nothing rnm
 where
  rnm = getRequiresNetworkMagic Dummy.aProtocolMagic

  genesisData = Genesis.GenesisData
    { Genesis.gdGenesisKeyHashes = Genesis.GenesisKeyHashes genesisKeyHashes
    , Genesis.gdHeavyDelegation = Genesis.UnsafeGenesisDelegation [] -- We don't need initial heavyweight delegation.
    , Genesis.gdStartTime = UTCTime (ModifiedJulianDay 0) 0
    , Genesis.gdNonAvvmBalances = Genesis.GenesisNonAvvmBalances []
    , Genesis.gdProtocolParameters = gPps
    , Genesis.gdK = BlockCount $ Abstract.unBlockCount stableAfter
    , Genesis.gdProtocolMagicId = Dummy.protocolMagicId
    , Genesis.gdAvvmDistr = Genesis.GenesisAvvmBalances []
    }

  -- We shouldn't need to use 'coerce' after
  -- https://github.com/input-output-hk/cardano-ledger/issues/332 gets
  -- implemented.
  genesisHash = Genesis.GenesisHash $ coerce $ H.hash ("" :: ByteString)

  gPps        = Update.ProtocolParameters
    { Update.ppScriptVersion    = 0
    , Update.ppSlotDuration     = 0
    , Update.ppMaxBlockSize     = 832 * protocolParams ^. maxBkSz
    , Update.ppMaxHeaderSize    = 569 * protocolParams ^. maxHdrSz
    , Update.ppMaxTxSize        = 318 * protocolParams ^. maxTxSz
    , Update.ppMaxProposalSize  = 0
    , Update.ppMpcThd           = LovelacePortion 0
    , Update.ppHeavyDelThd      = LovelacePortion 0
    , Update.ppUpdateVoteThd    = LovelacePortion 0
    , Update.ppUpdateProposalThd = LovelacePortion 0
    , Update.ppUpdateProposalTTL = 0
    , Update.ppSoftforkRule      = Update.SoftforkRule
      (LovelacePortion 0)
      (LovelacePortion 0)
      (LovelacePortion 0)
    , Update.ppTxFeePolicy       = TxFeePolicyTxSizeLinear
      $ TxSizeLinear (mkKnownLovelace @0) (mkKnownLovelace @0)
    , Update.ppUnlockStakeEpoch  = 0
    }

  genesisKeyHashes :: Set Common.KeyHash
  genesisKeyHashes =
    Set.map (hashKey . elaborateVKeyGenesis) allowedDelegators
