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
-- rules for the blockchain (also called the blockchain specification).
module Test.Cardano.Chain.Elaboration.Block
  ( abEnvToCfg
  , elaborate
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
import Data.Coerce (coerce)
import qualified Data.Map as Map
import Data.Monoid.Generic (GenericSemigroup (GenericSemigroup), GenericMonoid (GenericMonoid))
import qualified Data.Set as Set
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime))
import GHC.Generics (Generic)

import qualified Cardano.Crypto.Hashing as H
import Cardano.Crypto.ProtocolMagic (ProtocolMagic(..))

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
import qualified Ledger.Update as Abstract.Update
import qualified Ledger.UTxO as Abstract
import Cardano.Chain.Common
  ( BlockCount(BlockCount)
  , ChainDifficulty(ChainDifficulty)
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
  , elaboratePParams
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
  ( Concrete.Block (recomputeHashes bh0) bb0
  , AbstractToConcreteIdMaps
    { transactionIds = txIdMap'
    , proposalIds = proposalsIdMap'
    }
  )
 where
  AbstractToConcreteIdMaps txIdMap proposalsIdMap = abstractToConcreteIdMaps

  pm = Genesis.configProtocolMagicId config

  epochSlots = Genesis.configEpochSlots config

  bh0 = Concrete.mkHeaderExplicit
    pm
    prevHash
    (ChainDifficulty 0)
    epochSlots
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

  bb0    = Concrete.Body
             (UTxO.TxPayload txPayload)
             Ssc.SscPayload
             (Delegation.UnsafePayload dcerts)
             updatePayload

  dcerts =
    abstractBlock
      ^.. (Abstract.bBody . Abstract.bDCerts . traverse . to
            (elaborateDCert pm)
          )

  (txPayload, txIdMap') = elaborateTxWitnesses
    txIdMap
    (abstractBlock ^. Abstract.bBody . Abstract.bUtxo)

  updatePayload :: Update.Payload
  updatePayload =
    Update.Payload
      (fmap snd maybeProposals)
      (fmap (elaborateVote pm proposalsIdMap')
      $ Abstract._bUpdVotes
      $ Abstract._bBody abstractBlock
      )

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


  -- | Recompute the block header hashes (which correspond to the block
  -- payload) if the abstract hashes don't match the abstract payload.
  recomputeHashes :: Concrete.Header -> Concrete.Header
  recomputeHashes concreteHeader =
    concreteHeader {Concrete.headerProof = alteredHdrProof}
    where
      alteredHdrProof :: Concrete.Proof
      alteredHdrProof =
        originalHeaderProof
          { Concrete.proofDelegation = possiblyAlteredDelegationProof
          , Concrete.proofUpdate = possiblyAlteredUpdateProof
          , Concrete.proofUTxO = possiblyAlteredUTxOProof
          }
        where
          originalHeaderProof :: Concrete.Proof
          originalHeaderProof = Concrete.headerProof concreteHeader

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
      dummyHash = H.hash 0
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
  Genesis.Config {
      Genesis.configGenesisData       = genesisData
    , Genesis.configGenesisHash       = genesisHash
    , Genesis.configReqNetMagic       = rnm
    , Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
    }
 where
  rnm = getRequiresNetworkMagic Dummy.protocolMagic

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

  gPps = elaboratePParams protocolParams

  genesisKeyHashes :: Set Common.KeyHash
  genesisKeyHashes =
    Set.map (hashKey . elaborateVKeyGenesis) allowedDelegators
