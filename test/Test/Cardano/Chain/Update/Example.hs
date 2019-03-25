{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Update.Example
  ( exampleApplicationName
  , exampleProtocolVersion
  , exampleProtocolParameters
  , exampleProtocolParametersUpdate
  , exampleSoftwareVersion
  , exampleSystemTag
  , exampleUpdateData
  , examplePayload
  , exampleProof
  , exampleProposal
  , exampleProposalBody
  , exampleVote
  , exampleUndo
  , exampleUpId
  , exampleVoteId
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime)

import Cardano.Binary.Class (Raw(..))
import Cardano.Chain.Common
  (LovelacePortion(..), TxFeePolicy(..), TxSizeLinear(..), mkKnownLovelace)
import Cardano.Chain.Slotting (EpochIndex(..), FlatSlotId(..))
import Cardano.Chain.Update
  ( ApplicationName(..)
  , Payload
  , PrevValue(..)
  , Proof
  , Proposal
  , ProposalBody(..)
  , ProtocolParametersUpdate(..)
  , ProtocolParameters(..)
  , ProtocolVersion(..)
  , SoftforkRule(..)
  , SoftwareVersion(..)
  , SystemTag(..)
  , USUndo(..)
  , UpId
  , UpdateData(..)
  , Vote
  , VoteId
  , mkProof
  , mkVoteSafe
  , payload
  , signProposal
  )
import Cardano.Crypto (ProtocolMagicId(..), hash)

import Test.Cardano.Chain.Common.Example (exampleAttributes)
import Test.Cardano.Chain.Slotting.Example (exampleSlottingData)
import Test.Cardano.Crypto.Bi (getBytes)
import Test.Cardano.Crypto.Example (examplePublicKey, exampleSafeSigner)


exampleApplicationName :: ApplicationName
exampleApplicationName = ApplicationName "Golden"

exampleProtocolVersion :: ProtocolVersion
exampleProtocolVersion = ProtocolVersion 1 1 1

exampleProtocolParameters :: ProtocolParameters
exampleProtocolParameters = ProtocolParameters
  (999 :: Word16)
  (999e-6 :: NominalDiffTime)
  (999 :: Natural)
  (999 :: Natural)
  (999 :: Natural)
  (999 :: Natural)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (FlatSlotId 99)
  sfrule
  (TxFeePolicyTxSizeLinear tslin)
  (EpochIndex 99)
 where
  tslin  = TxSizeLinear c1' c2'
  c1'    = mkKnownLovelace @999
  c2'    = mkKnownLovelace @77
  sfrule = SoftforkRule
    (LovelacePortion 99)
    (LovelacePortion 99)
    (LovelacePortion 99)

exampleProtocolParametersUpdate :: ProtocolParametersUpdate
exampleProtocolParametersUpdate = ProtocolParametersUpdate
  (Just (999 :: Word16))
  (Just (999e-6 :: NominalDiffTime))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just $ FlatSlotId 99)
  (Just sfrule')
  (Just $ TxFeePolicyTxSizeLinear tslin')
  (Just $ EpochIndex 99)
 where
  tslin'  = TxSizeLinear co1 co2
  co1     = mkKnownLovelace @999
  co2     = mkKnownLovelace @77
  sfrule' = SoftforkRule
    (LovelacePortion 99)
    (LovelacePortion 99)
    (LovelacePortion 99)

exampleSystemTag :: SystemTag
exampleSystemTag = exampleSystemTags 0 1 !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count = map
  (toSystemTag . (* offset))
  [0 .. count - 1]
  where toSystemTag start = SystemTag (getText start 16)

exampleUndo :: USUndo
exampleUndo = USUndo
  { unChangedBV        = Map.singleton exampleProtocolVersion NoExist
  , unLastAdoptedBV    = Just exampleProtocolVersion
  , unChangedProps     = Map.singleton exampleUpId NoExist
  , unChangedSV        = Map.singleton exampleApplicationName NoExist
  , unChangedConfProps = Map.singleton exampleSoftwareVersion NoExist
  , unPrevProposers    = Nothing
  , unSlottingData     = Just exampleSlottingData
  }

exampleUpdateData :: UpdateData
exampleUpdateData = exampleUpdateDatas 10 2 !! 1

exampleUpdateDatas :: Int -> Int -> [UpdateData]
exampleUpdateDatas offset count = map
  (toUpdateData . (* offset))
  [0 .. count - 1]
 where
  toUpdateData start =
    let h = hash $ Raw (getBytes start 128) in UpdateData h h h h

exampleUpId :: UpId
exampleUpId = hash exampleProposal

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
exampleProposalBody = ProposalBody bv bvm sv hm ua
 where
  bv  = exampleProtocolVersion
  bvm = exampleProtocolParametersUpdate
  sv  = exampleSoftwareVersion
  hm  = Map.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
  ua  = exampleAttributes

exampleVote :: Vote
exampleVote = mkVoteSafe pm ss ui ar
 where
  pm = ProtocolMagicId 0
  ss = exampleSafeSigner 0
  ui = exampleUpId
  ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99
