{-# LANGUAGE PatternSynonyms #-}

module Test.Cardano.Chain.Block.Gen
  ( genBlockSignature
  , genHeaderHash
  , genHeader
  , genBody
  , genProof
  , genToSign
  , genBlock
  , genBlockWithEpochSlots
  , genBoundaryBlock
  , genBoundaryHeader
  )
where

import Cardano.Prelude

import Data.Coerce (coerce)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Chain.Block
  ( BlockSignature(..)
  , Block
  , Body (..)
  , BoundaryBlock(..)
  , BoundaryBody(..)
  , BoundaryHeader(..)
  , mkBoundaryHeader
  , Header
  , HeaderHash
  , Proof(..)
  , ToSign(..)
  , hashHeader
  , mkBlockExplicit
  , mkHeaderExplicit
  )
import Cardano.Chain.Delegation (signCertificate)
import Cardano.Chain.Genesis (GenesisHash(..))
import Cardano.Chain.Slotting
  (EpochNumber(..), EpochSlots, WithEpochSlots(WithEpochSlots))
import Cardano.Chain.Ssc (SscPayload(..), SscProof(..))
import Cardano.Crypto
  ( ProtocolMagicId
  , SignTag(SignBlock)
  , noPassSafeSigner
  , safeToVerification
  , sign
  , toVerification
  )

import Test.Cardano.Chain.Common.Gen
  (genChainDifficulty)
import qualified Test.Cardano.Chain.Delegation.Gen as Delegation
import Test.Cardano.Chain.Slotting.Gen
  (genEpochNumber, genEpochSlots, genSlotNumber, genEpochAndSlotCount)
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
    <*> genEpochNumber
    <*> genToSign pm epochSlots
 where
  mkBlockSignature issuerSafeSigner delegateSK epoch toSign =
    let
      cert =
        signCertificate pm (toVerification delegateSK) epoch issuerSafeSigner
      issuerVK = safeToVerification issuerSafeSigner
      sig      = sign pm (SignBlock issuerVK) delegateSK toSign
    in BlockSignature cert sig

genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash

genBody :: ProtocolMagicId -> Gen Body
genBody pm =
  Body
    <$> genTxPayload pm
    <*> pure SscPayload
    <*> Delegation.genPayload pm
    <*> Update.genPayload pm

genHeader :: ProtocolMagicId -> EpochSlots -> Gen Header
genHeader protocolMagicId epochSlots =
  mkHeaderExplicit'
    <$> genHeaderHash
    <*> genChainDifficulty
    <*> genSlotNumber
    <*> genBody protocolMagicId
    <*> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
    <*> genSigningKey
 where
  mkHeaderExplicit'
    headerHash
    chainDifficulty
    slotNumber
    body
    protocolVersion
    softwareVersion
    signingKey =
      mkHeaderExplicit
        protocolMagicId
        headerHash
        chainDifficulty
        epochSlots
        slotNumber
        signingKey
        (signCertificate
          protocolMagicId
          (toVerification signingKey)
          (EpochNumber 0)
          (noPassSafeSigner signingKey)
        )
        body
        protocolVersion
        softwareVersion

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
    <$> (hashHeader <$> genHeader pm epochSlots)
    <*> genProof pm
    <*> genEpochAndSlotCount epochSlots
    <*> genChainDifficulty
    <*> Update.genProtocolVersion
    <*> Update.genSoftwareVersion

genBlockWithEpochSlots :: ProtocolMagicId -> Gen (WithEpochSlots Block)
genBlockWithEpochSlots pm = do
  epochSlots <- genEpochSlots
  WithEpochSlots epochSlots <$> genBlock pm epochSlots

genBlock :: ProtocolMagicId -> EpochSlots -> Gen Block
genBlock protocolMagicId epochSlots =
  mkBlockExplicit'
    <$> Update.genProtocolVersion
    <*> Update.genSoftwareVersion
    <*> genHeaderHash
    <*> genChainDifficulty
    <*> genSlotNumber
    <*> genBody protocolMagicId
    <*> genSigningKey
 where
  mkBlockExplicit'
    protocolVersion
    softwareVersion
    headerHash
    chainDifficulty
    slotNumber
    body
    signingKey =
      mkBlockExplicit
        protocolMagicId
        protocolVersion
        softwareVersion
        headerHash
        chainDifficulty
        epochSlots
        slotNumber
        signingKey
        (signCertificate
          protocolMagicId
          (toVerification signingKey)
          (EpochNumber 0)
          (noPassSafeSigner signingKey)
        )
        body

genBoundaryBlock :: ProtocolMagicId -> Gen BoundaryBlock
genBoundaryBlock pm =
  BoundaryBlock
    <$> genBoundaryHeader pm
    <*> pure BoundaryBody

genBoundaryHeader :: ProtocolMagicId -> Gen BoundaryHeader
genBoundaryHeader pm = do
  epoch <- Gen.word64 (Range.exponential 0 maxBound)
  mkBoundaryHeader pm
    <$> ( if epoch == 0
          then Left . GenesisHash . coerce <$> genTextHash
          else Gen.choice [ Right <$> genHeaderHash
                          , Left . GenesisHash . coerce <$> genTextHash
                          ]
        )
    <*> pure epoch
    <*> genChainDifficulty
