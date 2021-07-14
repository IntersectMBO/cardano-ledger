{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Chain.Byron.API.Mempool
  ( ApplyMempoolPayloadErr (..),
    applyMempoolPayload,
    mempoolPayloadRecoverBytes,
    mempoolPayloadReencode,
  )
where

import Cardano.Binary
import qualified Cardano.Chain.Block as CC
import Cardano.Chain.Byron.API.Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Delegation.Validation.Interface as D.Iface
import qualified Cardano.Chain.Delegation.Validation.Scheduling as D.Sched
import qualified Cardano.Chain.Genesis as Gen
import qualified Cardano.Chain.MempoolPayload as CC
import qualified Cardano.Chain.Slotting as CC
import qualified Cardano.Chain.UTxO as Utxo
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.Update.Validation.Interface as U.Iface
import qualified Cardano.Chain.ValidationMode as CC
import Cardano.Crypto.ProtocolMagic
import Cardano.Prelude
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
  Apply any kind of transactions
-------------------------------------------------------------------------------}

-- | Errors that arise from applying an arbitrary mempool payload
--
-- Although @cardano-legder@ defines 'MempoolPayload', it does not define a
-- corresponding error type. We could 'ChainValidationError', but it's too
-- large, which is problematic because we actually sent encoded versions of
-- these errors across the wire.
data ApplyMempoolPayloadErr
  = MempoolTxErr Utxo.UTxOValidationError
  | MempoolDlgErr D.Sched.Error
  | MempoolUpdateProposalErr U.Iface.Error
  | MempoolUpdateVoteErr U.Iface.Error
  deriving (Eq, Show)

instance ToCBOR ApplyMempoolPayloadErr where
  toCBOR (MempoolTxErr err) =
    CBOR.encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR err
  toCBOR (MempoolDlgErr err) =
    CBOR.encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR err
  toCBOR (MempoolUpdateProposalErr err) =
    CBOR.encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR err
  toCBOR (MempoolUpdateVoteErr err) =
    CBOR.encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR err

instance FromCBOR ApplyMempoolPayloadErr where
  fromCBOR = do
    enforceSize "ApplyMempoolPayloadErr" 2
    CBOR.decodeWord8 >>= \case
      0 -> MempoolTxErr <$> fromCBOR
      1 -> MempoolDlgErr <$> fromCBOR
      2 -> MempoolUpdateProposalErr <$> fromCBOR
      3 -> MempoolUpdateVoteErr <$> fromCBOR
      tag -> cborError $ DecoderErrorUnknownTag "ApplyMempoolPayloadErr" tag

applyMempoolPayload ::
  MonadError ApplyMempoolPayloadErr m =>
  CC.ValidationMode ->
  Gen.Config ->
  CC.SlotNumber ->
  CC.AMempoolPayload ByteString ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
applyMempoolPayload validationMode cfg currentSlot payload =
  case payload of
    CC.MempoolTx tx ->
      (`wrapError` MempoolTxErr)
        . applyTxAux validationMode cfg [tx]
    CC.MempoolDlg cert ->
      (`wrapError` MempoolDlgErr)
        . applyCertificate cfg currentSlot [cert]
    CC.MempoolUpdateProposal proposal ->
      (`wrapError` MempoolUpdateProposalErr)
        . applyUpdateProposal cfg currentSlot proposal
    CC.MempoolUpdateVote vote ->
      (`wrapError` MempoolUpdateVoteErr)
        . applyUpdateVote cfg currentSlot vote

-- | The encoding of the mempool payload (without a 'AMempoolPayload' envelope)
mempoolPayloadRecoverBytes :: CC.AMempoolPayload ByteString -> ByteString
mempoolPayloadRecoverBytes = go
  where
    go :: CC.AMempoolPayload ByteString -> ByteString
    go (CC.MempoolTx payload) = recoverBytes payload
    go (CC.MempoolDlg payload) = recoverBytes payload
    go (CC.MempoolUpdateProposal payload) = recoverBytes payload
    go (CC.MempoolUpdateVote payload) = recoverBytes payload

-- | Re-encode the mempool payload (without any envelope)
mempoolPayloadReencode :: CC.AMempoolPayload a -> ByteString
mempoolPayloadReencode = go
  where
    go :: forall a. CC.AMempoolPayload a -> ByteString
    go (CC.MempoolTx payload) = reencode payload
    go (CC.MempoolDlg payload) = reencode payload
    go (CC.MempoolUpdateProposal payload) = reencode payload
    go (CC.MempoolUpdateVote payload) = reencode payload

    reencode :: (Functor f, ToCBOR (f ())) => f a -> ByteString
    reencode = CBOR.toStrictByteString . toCBOR . void

{-------------------------------------------------------------------------------
  Applying transactions
-------------------------------------------------------------------------------}

mkUtxoEnvironment ::
  Gen.Config ->
  CC.ChainValidationState ->
  Utxo.Environment
mkUtxoEnvironment cfg cvs =
  Utxo.Environment
    { Utxo.protocolMagic = protocolMagic,
      Utxo.protocolParameters = U.Iface.adoptedProtocolParameters updateState,
      Utxo.utxoConfiguration = Gen.configUTxOConfiguration cfg
    }
  where
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    updateState = CC.cvsUpdateState cvs

mkDelegationEnvironment ::
  Gen.Config ->
  CC.SlotNumber ->
  D.Iface.Environment
mkDelegationEnvironment cfg currentSlot =
  D.Iface.Environment
    { D.Iface.protocolMagic = getAProtocolMagicId protocolMagic,
      D.Iface.allowedDelegators = allowedDelegators cfg,
      D.Iface.k = k,
      -- The @currentSlot@/@currentEpoch@ for checking a delegation certificate
      -- must be that of the block in which the delegation certificate is/will
      -- be included.
      D.Iface.currentEpoch = currentEpoch,
      D.Iface.currentSlot = currentSlot
    }
  where
    k = Gen.configK cfg
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    currentEpoch = CC.slotNumberEpoch (Gen.configEpochSlots cfg) currentSlot

mkUpdateEnvironment ::
  Gen.Config ->
  CC.SlotNumber ->
  Delegation.Map ->
  U.Iface.Environment
mkUpdateEnvironment cfg currentSlot delegationMap =
  U.Iface.Environment
    { U.Iface.protocolMagic = getAProtocolMagicId protocolMagic,
      U.Iface.k = k,
      U.Iface.currentSlot = currentSlot,
      U.Iface.numGenKeys = numGenKeys,
      U.Iface.delegationMap = delegationMap
    }
  where
    k = Gen.configK cfg
    protocolMagic = reAnnotateMagic (Gen.configProtocolMagic cfg)
    numGenKeys = toNumGenKeys $ Set.size (allowedDelegators cfg)

    toNumGenKeys :: Int -> Word8
    toNumGenKeys n
      | n > fromIntegral (maxBound :: Word8) =
        panic $
          "toNumGenKeys: Too many genesis keys"
      | otherwise = fromIntegral n

applyTxAux ::
  MonadError Utxo.UTxOValidationError m =>
  CC.ValidationMode ->
  Gen.Config ->
  [Utxo.ATxAux ByteString] ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
applyTxAux validationMode cfg txs cvs =
  flip runReaderT validationMode $
    (`setUTxO` cvs)
      <$> Utxo.updateUTxO utxoEnv utxo txs
  where
    utxoEnv = mkUtxoEnvironment cfg cvs
    utxo = CC.cvsUtxo cvs

applyCertificate ::
  MonadError D.Sched.Error m =>
  Gen.Config ->
  CC.SlotNumber ->
  [Delegation.ACertificate ByteString] ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
applyCertificate cfg currentSlot certs cvs =
  (`setDelegationState` cvs)
    <$> D.Iface.updateDelegation dlgEnv dlgState certs
  where
    dlgEnv = mkDelegationEnvironment cfg currentSlot
    dlgState = CC.cvsDelegationState cvs

applyUpdateProposal ::
  MonadError U.Iface.Error m =>
  Gen.Config ->
  CC.SlotNumber ->
  Update.AProposal ByteString ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
applyUpdateProposal cfg currentSlot proposal cvs =
  (`setUpdateState` cvs)
    <$> U.Iface.registerProposal updateEnv updateState proposal
  where
    updateEnv = mkUpdateEnvironment cfg currentSlot (getDelegationMap cvs)
    updateState = CC.cvsUpdateState cvs

applyUpdateVote ::
  MonadError U.Iface.Error m =>
  Gen.Config ->
  CC.SlotNumber ->
  Update.AVote ByteString ->
  CC.ChainValidationState ->
  m CC.ChainValidationState
applyUpdateVote cfg currentSlot vote cvs =
  (`setUpdateState` cvs)
    <$> U.Iface.registerVote updateEnv updateState vote
  where
    updateEnv = mkUpdateEnvironment cfg currentSlot (getDelegationMap cvs)
    updateState = CC.cvsUpdateState cvs

{-------------------------------------------------------------------------------
  Update parts of the chain state
-------------------------------------------------------------------------------}

setUTxO ::
  Utxo.UTxO ->
  CC.ChainValidationState ->
  CC.ChainValidationState
setUTxO newUTxO cvs = cvs {CC.cvsUtxo = newUTxO}

setDelegationState ::
  D.Iface.State ->
  CC.ChainValidationState ->
  CC.ChainValidationState
setDelegationState newDlg cvs = cvs {CC.cvsDelegationState = newDlg}

setUpdateState ::
  U.Iface.State ->
  CC.ChainValidationState ->
  CC.ChainValidationState
setUpdateState newUpdate cvs = cvs {CC.cvsUpdateState = newUpdate}
