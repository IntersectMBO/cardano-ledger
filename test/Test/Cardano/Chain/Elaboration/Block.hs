{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | This module provides functionality for translating abstract blocks into
-- concrete blocks. The abstract blocks are generated according the small-step
-- rules for the block chain (also called the blockchain specification).
module Test.Cardano.Chain.Elaboration.Block
  ( abEnvToCfg
  , elaborate
  , elaborateBS
  )
where

import Cardano.Prelude hiding (to)

import Control.Lens ((^.), to, (^..))
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime))

import qualified Cardano.Binary.Class as Binary
import qualified Cardano.Crypto.Hashing as H

import qualified Cardano.Chain.Block as Concrete
import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Ssc as Ssc
import qualified Cardano.Chain.Txp as Txp
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Slotting as Slotting

import qualified Control.State.Transition as Transition
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN, disL, epochL)
import qualified Cardano.Spec.Chain.STS.Block as Abstract
import qualified Ledger.Core as Abstract
import Ledger.Delegation (DCert, delegationMap, delegatorOf, mkDCert)
import Ledger.Update (bkSgnCntW, bkSlotsPerEpoch, maxBkSz, maxHdrSz)
import Cardano.Chain.Common
  ( BlockCount(BlockCount)
  , LovelacePortion(LovelacePortion)
  , TxFeePolicy(TxFeePolicyTxSizeLinear)
  , TxSizeLinear(TxSizeLinear)
  , mkKnownLovelace
  , mkStakeholderId
  )

import Test.Cardano.Chain.Elaboration.Keys
  (elaborateKeyPair, elaborateVKeyGenesis, vKeyPair)
import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)
import Test.Cardano.Chain.Elaboration.Delegation (elaborateDCert)

-- | Elaborate an abstract block into a concrete block (without annotations).
elaborate
  :: Genesis.Config
  -> Transition.Environment CHAIN
  -> Transition.State CHAIN
  -> Concrete.ChainValidationState
  -> Abstract.Block
  -> Concrete.ABlock ()
elaborate config (_, _, pps) ast st ab = Concrete.ABlock
  { Concrete.blockHeader     = bh0
  , Concrete.blockBody       = bb0
  , Concrete.aBlockExtraData = Binary.Annotated extraBodyData ()
  , Concrete.blockAnnotation = ()
  }
 where
  bh0 = Concrete.mkHeaderExplicit
    (Genesis.configProtocolMagicId config)
    prevHash
    0
    sid
    ssk
    cDCert
    bb0
    extraHeaderData

  emptyAttrs      = Common.mkAttributes ()

  extraBodyData   = Concrete.ExtraBodyData emptyAttrs

  -- Once the update mechanism is in place we might need to fill this in with
  -- the update data.
  extraHeaderData = Concrete.ExtraHeaderData
    { Concrete.ehdProtocolVersion = Update.ProtocolVersion 0 0 0
    , Concrete.ehdSoftwareVersion = Update.SoftwareVersion
      (Update.ApplicationName "baz")
      0
    , Concrete.ehdAttributes      = emptyAttrs
    , Concrete.ehdEBDataProof     = H.hash extraBodyData
    }

  prevHash = fromMaybe
    (Genesis.configGenesisHeaderHash config)
    (Concrete.cvsPreviousHash st)

  sid =
    Slotting.unflattenSlotId (coerce (pps ^. bkSlotsPerEpoch))
      $ Slotting.FlatSlotId
          (ab ^. Abstract.bHeader . Abstract.bSlot . to Abstract.unSlot)

  issuer   = ab ^. Abstract.bHeader . Abstract.bIssuer

  (_, ssk) = elaborateKeyPair $ vKeyPair issuer

  cDCert :: Maybe Delegation.Certificate
  cDCert = Just $ elaborateDCert config $ rcDCert issuer ast

  bb0    = Concrete.ABody
    { Concrete.bodyTxPayload     = Txp.ATxPayload []
    , Concrete.bodySscPayload    = Ssc.SscPayload
    , Concrete.bodyDlgPayload    = Delegation.UnsafeAPayload dcerts ()
    , Concrete.bodyUpdatePayload = Update.APayload Nothing [] ()
    }

  dcerts =
    ab
      ^.. (Abstract.bBody . Abstract.bDCerts . traverse . to
            (elaborateDCert config)
          )

elaborateBS
  :: Genesis.Config -- TODO: Do we want this to come from the abstract
                    -- environment? (in such case we wouldn't need this
                    -- parameter)
  -> Transition.Environment CHAIN
  -> Transition.State CHAIN
  -> Concrete.ChainValidationState
  -> Abstract.Block
  -> Concrete.ABlock ByteString
elaborateBS config aenv ast st ab =
  annotateBlock $ elaborate config aenv ast st ab

annotateBlock :: Concrete.Block -> Concrete.ABlock ByteString
annotateBlock block =
  let
    decodedABlockOrBoundary =
      case
          Binary.decodeFullDecoder "Block" Concrete.decodeABlockOrBoundary bytes
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
  where bytes = Binary.serializeEncoding (Concrete.encodeBlock block)

-- | Re-construct an abstract delegation certificate from the abstract state.
--
-- We need to do this because the delegation certificate is included in the
-- block.
rcDCert
  :: Abstract.VKey
  -- ^ Key for which the delegation certificate is being constructed.
  -> Transition.State CHAIN
  -> DCert
rcDCert vk ast = mkDCert vkg sigVkg vk (ast ^. epochL)
 where
  dm :: Map Abstract.VKeyGenesis Abstract.VKey
  dm  = ast ^. disL . delegationMap

  vkg = fromMaybe err $ delegatorOf dm vk

  err :: Abstract.VKeyGenesis
  err    = panic $ "No delegator found for key " <> show vk

  vkp    = vKeyPair $ coerce vkg

  sigVkg = Abstract.sign (Abstract.sKey vkp) vkg

--  | Make a genesis configuration from an initial abstract environment of the
--  | trace.
--
abEnvToCfg :: Transition.Environment CHAIN -> Genesis.Config
abEnvToCfg (_, vkgs, pps) = Genesis.Config genesisData genesisHash Nothing
 where
  genesisData = Genesis.GenesisData
    { Genesis.gdBootStakeholders = Genesis.GenesisWStakeholders
      genesisStakeHolders
    , Genesis.gdHeavyDelegation = Genesis.UnsafeGenesisDelegation [] -- We don't need initial heavyweight delegation.
    , Genesis.gdStartTime = UTCTime (ModifiedJulianDay 0) 0
    , Genesis.gdNonAvvmBalances = Genesis.GenesisNonAvvmBalances []
    , Genesis.gdProtocolParameters = gPps
    , Genesis.gdK         =
        -- TODO: this should be a different protocol parameter once we have
        -- an abstract protocol parameter for k. Then we need to solve the
        -- problem that in the concrete implementation k and w are the same.
                            BlockCount (fromIntegral $ pps ^. bkSgnCntW)
    , Genesis.gdProtocolMagic = dummyProtocolMagic
    , Genesis.gdAvvmDistr = Genesis.GenesisAvvmBalances []
    }

  -- We shouldn't need to use 'coerce' after
  -- https://github.com/input-output-hk/cardano-ledger/issues/332 gets
  -- implemented.
  genesisHash = Genesis.GenesisHash $ coerce $ H.hash ("" :: ByteString)

  gPps        = Update.ProtocolParameters
    { Update.ppScriptVersion    = 0
    , Update.ppSlotDuration     = 0
    , Update.ppMaxBlockSize     = pps ^. maxBkSz
    , Update.ppMaxHeaderSize    = pps ^. maxHdrSz
    , Update.ppMaxTxSize        = 0
    , Update.ppMaxProposalSize  = 0
    , Update.ppMpcThd           = LovelacePortion 0
    , Update.ppHeavyDelThd      = LovelacePortion 0
    , Update.ppUpdateVoteThd    = LovelacePortion 0
    , Update.ppUpdateProposalThd = LovelacePortion 0
    , Update.ppUpdateImplicit   = 0
    , Update.ppSoftforkRule     = Update.SoftforkRule
      (LovelacePortion 0)
      (LovelacePortion 0)
      (LovelacePortion 0)
    , Update.ppTxFeePolicy      = TxFeePolicyTxSizeLinear
      $ TxSizeLinear (mkKnownLovelace @0) (mkKnownLovelace @0)
    , Update.ppUnlockStakeEpoch = 0
    }


  genesisStakeHolders :: Map Common.StakeholderId Word16
  genesisStakeHolders = Map.fromList
    $ zip (mkStakeholderId . elaborateVKeyGenesis <$> vkgs') [1 ..]

  vkgs' = Set.toList vkgs
