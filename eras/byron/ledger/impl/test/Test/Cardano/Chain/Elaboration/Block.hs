{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides functionality for translating abstract blocks into
-- concrete blocks. The abstract blocks are generated according the small-step
-- rules for the blockchain (also called the blockchain specification).
module Test.Cardano.Chain.Elaboration.Block
  ( abEnvToCfg,
    elaborate,
    elaborateBS,
    rcDCert,
    AbstractToConcreteIdMaps
      ( AbstractToConcreteIdMaps,
        proposalIds,
        transactionIds
      ),
  )
where

import qualified Byron.Spec.Chain.STS.Block as Abstract
import Byron.Spec.Chain.STS.Rule.Chain (CHAIN, disL)
import qualified Byron.Spec.Chain.STS.Rule.Epoch as Abstract
import qualified Byron.Spec.Ledger.Core as Abstract
import Byron.Spec.Ledger.Delegation
  ( DCert,
    delegationMap,
    delegatorOf,
    mkDCert,
  )
import qualified Byron.Spec.Ledger.UTxO as Abstract
import qualified Byron.Spec.Ledger.Update as Abstract.Update
import qualified Cardano.Binary as Binary
import qualified Cardano.Chain.Block as Concrete
import Cardano.Chain.Common
  ( BlockCount (BlockCount),
    ChainDifficulty (ChainDifficulty),
    hashKey,
  )
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Slotting as Slotting
import qualified Cardano.Chain.Ssc as Ssc
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Crypto.Hashing as H
import Cardano.Crypto.ProtocolMagic (AProtocolMagic (..))
import Cardano.Prelude hiding (to)
import Control.Arrow ((&&&))
import qualified Control.State.Transition as Transition
import Data.Bimap (Bimap)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid.Generic (GenericMonoid (GenericMonoid), GenericSemigroup (GenericSemigroup))
import qualified Data.Set as Set
import Data.Time (Day (ModifiedJulianDay), UTCTime (UTCTime))
import Lens.Micro (to, (^.), (^..))
import Test.Cardano.Chain.Elaboration.Delegation (elaborateDCert)
import Test.Cardano.Chain.Elaboration.Keys
  ( elaborateVKeyGenesis,
    vKeyPair,
    vKeyToSKey,
  )
import Test.Cardano.Chain.Elaboration.Update
  ( elaboratePParams,
    elaborateProtocolVersion,
    elaborateSoftwareVersion,
    elaborateUpdateProposal,
    elaborateVote,
  )
import Test.Cardano.Chain.UTxO.Model (elaborateTxWitnesses)
import qualified Test.Cardano.Crypto.Dummy as Dummy

data AbstractToConcreteIdMaps = AbstractToConcreteIdMaps
  { transactionIds :: !(Map Abstract.TxId UTxO.TxId),
    proposalIds :: !(Map Abstract.Update.UpId Update.UpId)
  }
  deriving (Eq, Show, Generic)
  deriving (Semigroup) via GenericSemigroup AbstractToConcreteIdMaps
  deriving (Monoid) via GenericMonoid AbstractToConcreteIdMaps

-- | Elaborate an abstract block into a concrete block (without annotations).
elaborate ::
  AbstractToConcreteIdMaps ->
  Genesis.Config ->
  DCert ->
  Concrete.ChainValidationState ->
  Abstract.Block ->
  (Concrete.Block, AbstractToConcreteIdMaps)
elaborate abstractToConcreteIdMaps config dCert st abstractBlock =
  ( Concrete.ABlock
      { Concrete.blockHeader = recomputeHashes bh0,
        Concrete.blockBody = bb0,
        Concrete.blockAnnotation = ()
      },
    AbstractToConcreteIdMaps
      { transactionIds = txIdMap',
        proposalIds = proposalsIdMap'
      }
  )
  where
    AbstractToConcreteIdMaps txIdMap proposalsIdMap = abstractToConcreteIdMaps

    pm = Genesis.configProtocolMagicId config

    bh0 =
      Concrete.mkHeaderExplicit
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

    sid =
      Slotting.SlotNumber
        (abstractBlock ^. Abstract.bHeader . Abstract.bhSlot . to Abstract.unSlot)

    issuer = abstractBlock ^. Abstract.bHeader . Abstract.bhIssuer

    ssk = vKeyToSKey issuer

    cDCert :: Delegation.Certificate
    cDCert = elaborateDCert pm dCert

    bb0 =
      Concrete.ABody
        { Concrete.bodyTxPayload = UTxO.ATxPayload txPayload,
          Concrete.bodySscPayload = Ssc.SscPayload,
          Concrete.bodyDlgPayload = Delegation.UnsafeAPayload dcerts (),
          Concrete.bodyUpdatePayload = updatePayload
        }

    dcerts =
      abstractBlock
        ^.. ( Abstract.bBody . Abstract.bDCerts . traverse
                . to
                  (elaborateDCert pm)
            )

    (txPayload, txIdMap') =
      first (fmap void) $
        elaborateTxWitnesses
          txIdMap
          (abstractBlock ^. Abstract.bBody . Abstract.bUtxo)

    updatePayload :: Update.APayload ()
    updatePayload =
      Update.APayload
        (fmap snd maybeProposals)
        ( fmap (elaborateVote pm proposalsIdMap') $
            Abstract._bUpdVotes $
              Abstract._bBody abstractBlock
        )
        () -- Update payload annotation
    maybeProposals :: Maybe (Abstract.Update.UProp, Update.Proposal)
    maybeProposals =
      fmap (identity &&& elaborateUpdateProposal pm) $
        Abstract._bUpdProp $
          Abstract._bBody abstractBlock

    proposalsIdMap' :: Map Abstract.Update.UpId Update.UpId
    proposalsIdMap' = maybe proposalsIdMap addUpdateProposalId maybeProposals
      where
        addUpdateProposalId (abstractProposal, concreteProposal) =
          Map.insert
            (Abstract.Update._upId abstractProposal)
            (H.serializeCborHash concreteProposal)
            proposalsIdMap

    recomputeHashes :: Concrete.AHeader () -> Concrete.AHeader ()
    recomputeHashes concreteHeader =
      concreteHeader {Concrete.aHeaderProof = Binary.Annotated alteredHdrProof ()}
      where
        alteredHdrProof :: Concrete.Proof
        alteredHdrProof =
          originalHeaderProof
            { Concrete.proofDelegation = possiblyAlteredDelegationProof,
              Concrete.proofUpdate = possiblyAlteredUpdateProof,
              Concrete.proofUTxO = possiblyAlteredUTxOProof
            }
          where
            originalHeaderProof :: Concrete.Proof
            originalHeaderProof =
              Binary.unAnnotated (Concrete.aHeaderProof concreteHeader)

            possiblyAlteredUTxOProof :: UTxO.TxProof
            possiblyAlteredUTxOProof =
              if (Abstract.isValid $ Abstract._bhUtxoHash $ Abstract._bHeader abstractBlock)
                then originalUTxOProof
                else originalUTxOProof {UTxO.txpWitnessesHash = coerce dummyHash}
              where
                originalUTxOProof :: UTxO.TxProof
                originalUTxOProof = Concrete.proofUTxO originalHeaderProof

            possiblyAlteredDelegationProof :: H.Hash Delegation.Payload
            possiblyAlteredDelegationProof =
              if (Abstract.isValid $ Abstract._bhDlgHash $ Abstract._bHeader abstractBlock)
                then Concrete.proofDelegation originalHeaderProof
                else coerce dummyHash

            possiblyAlteredUpdateProof :: Update.Proof
            possiblyAlteredUpdateProof =
              if (Abstract.isValid $ Abstract._bhUpdHash $ Abstract._bHeader abstractBlock)
                then Concrete.proofUpdate originalHeaderProof
                else coerce dummyHash

        dummyHash :: H.Hash Int
        dummyHash = H.serializeCborHash 0

elaborateBS ::
  AbstractToConcreteIdMaps ->
  Genesis.Config -> -- TODO: Do we want this to come from the abstract
  -- environment? (in such case we wouldn't need this
  -- parameter)
  DCert ->
  Concrete.ChainValidationState ->
  Abstract.Block ->
  (Concrete.ABlock ByteString, AbstractToConcreteIdMaps)
elaborateBS txIdMap config dCert st ab =
  first (annotateBlock (Genesis.configEpochSlots config)) $
    elaborate txIdMap config dCert st ab

annotateBlock :: Slotting.EpochSlots -> Concrete.Block -> Concrete.ABlock ByteString
annotateBlock epochSlots block =
  let decodedABlockOrBoundary =
        case Binary.decodeFullDecoder
          "Block"
          (Concrete.fromCBORABlockOrBoundary epochSlots)
          bytes of
          Left err ->
            panic $
              "This function should be able to decode the block it encoded"
                <> ". Instead I got: "
                <> show err
          Right abobb -> map (LBS.toStrict . Binary.slice bytes) abobb
   in case decodedABlockOrBoundary of
        Concrete.ABOBBlock bk -> bk
        Concrete.ABOBBoundary _ ->
          panic "This function should have decoded a block."
  where
    bytes = Binary.serializeEncoding (Concrete.toCBORABOBBlock epochSlots block)

-- | Re-construct an abstract delegation certificate from the abstract state.
--
-- We need to do this because the delegation certificate is included in the
-- block.
rcDCert ::
  HasCallStack =>
  -- | Key for which the delegation certificate is being constructed.
  Abstract.VKey ->
  -- | Chain stability parameter
  Abstract.BlockCount ->
  Transition.State CHAIN ->
  DCert
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
abEnvToCfg :: Transition.Environment CHAIN -> Genesis.Config
abEnvToCfg (_currentSlot, _genesisUtxo, allowedDelegators, protocolParams, stableAfter) =
  Genesis.Config
    { Genesis.configGenesisData = genesisData,
      Genesis.configGenesisHash = genesisHash,
      Genesis.configReqNetMagic = rnm,
      Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
    }
  where
    rnm = getRequiresNetworkMagic Dummy.aProtocolMagic

    genesisData =
      Genesis.GenesisData
        { Genesis.gdGenesisKeyHashes = Genesis.GenesisKeyHashes genesisKeyHashes,
          Genesis.gdHeavyDelegation = Genesis.UnsafeGenesisDelegation [], -- We don't need initial heavyweight delegation.
          Genesis.gdStartTime = UTCTime (ModifiedJulianDay 0) 0,
          Genesis.gdNonAvvmBalances = Genesis.GenesisNonAvvmBalances [],
          Genesis.gdProtocolParameters = gPps,
          Genesis.gdK = BlockCount $ Abstract.unBlockCount stableAfter,
          Genesis.gdProtocolMagicId = Dummy.protocolMagicId,
          Genesis.gdAvvmDistr = Genesis.GenesisAvvmBalances []
        }

    -- We shouldn't need to use 'coerce' after
    -- https://github.com/input-output-hk/cardano-ledger/issues/332 gets
    -- implemented.
    genesisHash = Genesis.GenesisHash $ coerce $ H.serializeCborHash ("" :: ByteString)

    gPps = elaboratePParams protocolParams

    genesisKeyHashes :: Set Common.KeyHash
    genesisKeyHashes =
      Set.map (hashKey . elaborateVKeyGenesis) allowedDelegators
