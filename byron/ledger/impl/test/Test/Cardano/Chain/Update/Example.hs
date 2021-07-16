{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Chain.Update.Example
  ( exampleApplicationName,
    exampleProtocolVersion,
    exampleProtocolParameters,
    exampleProtocolParametersUpdate,
    exampleSoftwareVersion,
    exampleSystemTag,
    exampleInstallerHash,
    examplePayload,
    exampleProof,
    exampleProposal,
    exampleProposalBody,
    exampleUpId,
    exampleVote,
  )
where

import Cardano.Binary (Raw (..))
import Cardano.Chain.Common
  ( TxFeePolicy (..),
    TxSizeLinear (..),
    mkKnownLovelace,
    rationalToLovelacePortion,
  )
import Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import Cardano.Chain.Update
  ( ApplicationName (..),
    InstallerHash (..),
    Payload,
    Proof,
    Proposal,
    ProposalBody (..),
    ProtocolParameters (..),
    ProtocolParametersUpdate (..),
    ProtocolVersion (..),
    SoftforkRule (..),
    SoftwareVersion (..),
    SystemTag (..),
    UpId,
    Vote,
    mkProof,
    payload,
    signProposal,
    signVote,
  )
import Cardano.Crypto (ProtocolMagicId (..), serializeCborHash)
import Cardano.Prelude
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Test.Cardano.Crypto.CBOR (getBytes)
import Test.Cardano.Crypto.Example (exampleSafeSigner)
import Test.Cardano.Prelude

exampleApplicationName :: ApplicationName
exampleApplicationName = ApplicationName "Golden"

exampleProtocolVersion :: ProtocolVersion
exampleProtocolVersion = ProtocolVersion 1 1 1

exampleProtocolParameters :: ProtocolParameters
exampleProtocolParameters =
  ProtocolParameters
    (999 :: Word16)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (999 :: Natural)
    (rationalToLovelacePortion 99e-15)
    (rationalToLovelacePortion 99e-15)
    (rationalToLovelacePortion 99e-15)
    (rationalToLovelacePortion 99e-15)
    (SlotNumber 99)
    sfrule
    (TxFeePolicyTxSizeLinear tslin)
    (EpochNumber 99)
  where
    tslin = TxSizeLinear c1' c2'
    c1' = mkKnownLovelace @999
    c2' = 77 :: Rational
    sfrule =
      SoftforkRule
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)

exampleProtocolParametersUpdate :: ProtocolParametersUpdate
exampleProtocolParametersUpdate =
  ProtocolParametersUpdate
    (Just (999 :: Word16))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just (999 :: Natural))
    (Just $ rationalToLovelacePortion 99e-15)
    (Just $ rationalToLovelacePortion 99e-15)
    (Just $ rationalToLovelacePortion 99e-15)
    (Just $ rationalToLovelacePortion 99e-15)
    (Just $ SlotNumber 99)
    (Just sfrule')
    (Just $ TxFeePolicyTxSizeLinear tslin')
    (Just $ EpochNumber 99)
  where
    tslin' = TxSizeLinear co1 co2
    co1 = mkKnownLovelace @999
    co2 = 77 :: Rational
    sfrule' =
      SoftforkRule
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)
        (rationalToLovelacePortion 99e-15)

exampleSystemTag :: SystemTag
exampleSystemTag = exampleSystemTags 0 1 !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count =
  map
    (toSystemTag . (* offset))
    [0 .. count - 1]
  where
    toSystemTag start = SystemTag (getText start 16)

exampleInstallerHash :: InstallerHash
exampleInstallerHash = exampleInstallerHashes 10 2 !! 1

exampleInstallerHashes :: Int -> Int -> [InstallerHash]
exampleInstallerHashes offset count =
  map
    (toInstallerHash . (* offset))
    [0 .. count - 1]
  where
    toInstallerHash start = InstallerHash . serializeCborHash . Raw $ getBytes start 128

exampleUpId :: UpId
exampleUpId = serializeCborHash exampleProposal

examplePayload :: Payload
examplePayload = payload up uv
  where
    up = Just exampleProposal
    uv = [exampleVote]

exampleProof :: Proof
exampleProof = mkProof examplePayload

exampleProposal :: Proposal
exampleProposal = signProposal pm exampleProposalBody ss
  where
    pm = ProtocolMagicId 0
    ss = exampleSafeSigner 0

exampleProposalBody :: ProposalBody
exampleProposalBody = ProposalBody bv bvm sv hm
  where
    bv = exampleProtocolVersion
    bvm = exampleProtocolParametersUpdate
    sv = exampleSoftwareVersion
    hm =
      Map.fromList $ zip (exampleSystemTags 10 5) (exampleInstallerHashes 10 5)

exampleVote :: Vote
exampleVote = signVote pm ui ar ss
  where
    pm = ProtocolMagicId 0
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99
