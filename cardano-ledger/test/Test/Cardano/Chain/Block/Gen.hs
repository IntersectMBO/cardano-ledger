module Test.Cardano.Chain.Block.Gen
  ( genBlockSignature
  , genHeaderHash
  , genHeader
  , genBody
  , genProof
  , genToSign
  , genBlock
  , genBlockWithEpochSlots
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import Hedgehog (Gen)

import Cardano.Chain.Block
  ( ABlockSignature(..)
  , Block
  , BlockSignature
  , Body
  , Header
  , HeaderHash
  , Proof(..)
  , ToSign(..)
  , body
  , hashHeader
  , mkBlockExplicit
  , mkHeaderExplicit
  )
import Cardano.Chain.Delegation (mkCertificate)
import Cardano.Chain.Slotting
  (EpochIndex(..), EpochSlots, WithEpochSlots(WithEpochSlots))
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignBlock)
  , noPassSafeSigner
  , safeToVerification
  , sign
  , toVerification
  )

import Test.Cardano.Chain.Common.Gen (genChainDifficulty)
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import Test.Cardano.Chain.Slotting.Gen
  (genEpochIndex, genEpochSlots, genFlatSlotId, genSlotId)
import Test.Cardano.Chain.UTxO.Gen (genTxPayload, genTxProof)
import qualified Test.Cardano.Chain.Update.Gen as Update
import Test.Cardano.Crypto.Gen
  ( genAbstractHash
  , genSafeSigner
  , genSigningKey
  , genTextHash
  )


genBlockSignature :: ProtocolMagicId -> EpochSlots -> Gen BlockSignature
genBlockSignature pm epochSlots =
  mkBlockSignature
    <$> genSafeSigner
    <*> genSigningKey
    <*> genEpochIndex
    <*> genToSign pm epochSlots
 where
  mkBlockSignature issuerSafeSigner delegateSK epoch toSign =
    let
      cert =
        mkCertificate pm issuerSafeSigner (toVerification delegateSK) epoch
      issuerVK = safeToVerification issuerSafeSigner
      sig      = sign pm (SignBlock issuerVK) delegateSK toSign
    in ABlockSignature cert sig

genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash

genBody :: ProtocolMagicId -> Gen Body
genBody pm =
  body
    <$> genTxPayload pm
    <*> pure SscPayload
    <*> Delegation.genPayload pm
    <*> Update.genPayload pm

genHeader :: ProtocolMagicId -> EpochSlots -> Gen Header
genHeader pm epochSlots = do
  sk <- genSigningKey
  let
    cert =
      mkCertificate pm (noPassSafeSigner sk) (toVerification sk) (EpochIndex 0)
  mkHeaderExplicit pm
    <$> genHeaderHash
    <*> genChainDifficulty
    <*> pure epochSlots
    <*> genFlatSlotId
    <*> pure sk
    <*> pure cert
    <*> genBody pm
    <*> Update.genProtocolVersion
    <*> Update.genSoftwareVersion

genProof :: ProtocolMagicId -> Gen Proof
genProof pm =
  Proof
    <$> genTxProof pm
    <*> pure SscProof
    <*> genAbstractHash (Delegation.genPayload pm)
    <*> Update.genProof pm

genToSign :: ProtocolMagicId -> EpochSlots -> Gen ToSign
genToSign pm epochSlots =
  ToSign
    <$> (mkAbstractHash <$> genHeader pm epochSlots)
    <*> genProof pm
    <*> genSlotId epochSlots
    <*> genChainDifficulty
    <*> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
 where
  mkAbstractHash :: Header -> HeaderHash
  mkAbstractHash = hashHeader epochSlots

genBlockWithEpochSlots :: ProtocolMagicId -> Gen (WithEpochSlots Block)
genBlockWithEpochSlots pm = do
  epochSlots <- genEpochSlots
  b          <- genBlock pm epochSlots
  pure $! WithEpochSlots epochSlots b

genBlock :: ProtocolMagicId -> EpochSlots -> Gen Block
genBlock pm epochSlots = do
  sk <- genSigningKey
  let
    cert =
      mkCertificate pm (noPassSafeSigner sk) (toVerification sk) (EpochIndex 0)
  mkBlockExplicit pm
    <$> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
    <*> genHeaderHash
    <*> genChainDifficulty
    <*> pure epochSlots
    <*> genFlatSlotId
    <*> pure sk
    <*> pure cert
    <*> genBody pm
