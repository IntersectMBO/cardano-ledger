{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Update.Example
  ( exampleApplicationName
  , exampleBlockVersion
  , exampleBlockVersionData
  , exampleBlockVersionModifier
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

import Data.Fixed (Fixed(..))
import Data.List ((!!))
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime)

import Cardano.Binary.Class (Raw(..))
import Cardano.Chain.Common
  ( Coeff(..)
  , LovelacePortion(..)
  , ScriptVersion
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  )
import Cardano.Chain.Slotting (EpochIndex(..), FlatSlotId)
import Cardano.Chain.Update
  ( ApplicationName(..)
  , BlockVersion(..)
  , BlockVersionData(..)
  , BlockVersionModifier(..)
  , Payload(..)
  , PrevValue(..)
  , Proof
  , Proposal
  , ProposalBody(..)
  , SoftforkRule(..)
  , SoftwareVersion(..)
  , SystemTag(..)
  , USUndo(..)
  , UpId
  , UpdateData(..)
  , Vote(..)
  , VoteId
  , mkProof
  , mkVoteSafe
  , signProposal
  )
import Cardano.Crypto (ProtocolMagic(..), hash)

import Test.Cardano.Chain.Common.Example (exampleAttributes)
import Test.Cardano.Chain.Slotting.Example (exampleSlottingData)
import Test.Cardano.Crypto.Bi (getBytes)
import Test.Cardano.Crypto.Example (examplePublicKey, exampleSafeSigner)


exampleApplicationName :: ApplicationName
exampleApplicationName = ApplicationName "Golden"

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
  (999 :: ScriptVersion)
  (999e-6 :: NominalDiffTime)
  (999 :: Natural)
  (999 :: Natural)
  (999 :: Natural)
  (999 :: Natural)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (LovelacePortion 99)
  (99 :: FlatSlotId)
  sfrule
  (TxFeePolicyTxSizeLinear tslin)
  (EpochIndex 99)
 where
  tslin  = TxSizeLinear c1' c2'
  c1'    = Coeff (MkFixed 999)
  c2'    = Coeff (MkFixed 77)
  sfrule = SoftforkRule
    (LovelacePortion 99)
    (LovelacePortion 99)
    (LovelacePortion 99)

exampleBlockVersionModifier :: BlockVersionModifier
exampleBlockVersionModifier = BlockVersionModifier
  (Just (999 :: ScriptVersion))
  (Just (999e-6 :: NominalDiffTime))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just (999 :: Natural))
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just $ LovelacePortion 99)
  (Just (99 :: FlatSlotId))
  (Just sfrule')
  (Just $ TxFeePolicyTxSizeLinear tslin')
  (Just $ EpochIndex 99)
 where
  tslin'  = TxSizeLinear co1 co2
  co1     = Coeff (MkFixed 999)
  co2     = Coeff (MkFixed 77)
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
  { unChangedBV        = Map.singleton exampleBlockVersion NoExist
  , unLastAdoptedBV    = Just exampleBlockVersion
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
examplePayload = Payload up uv
 where
  up = Just exampleProposal
  uv = [exampleVote]

exampleProof :: Proof
exampleProof = mkProof examplePayload


exampleProposal :: Proposal
exampleProposal = signProposal pm exampleProposalBody ss
 where
  pm = ProtocolMagic 0
  ss = exampleSafeSigner 0

exampleProposalBody :: ProposalBody
exampleProposalBody = ProposalBody bv bvm sv hm ua
 where
  bv  = exampleBlockVersion
  bvm = exampleBlockVersionModifier
  sv  = exampleSoftwareVersion
  hm  = Map.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
  ua  = exampleAttributes

exampleVote :: Vote
exampleVote = mkVoteSafe pm ss ui ar
 where
  pm = ProtocolMagic 0
  ss = exampleSafeSigner 0
  ui = exampleUpId
  ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99
