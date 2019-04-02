{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module STS.Vrf
  ( VRF
  ) where

import qualified Data.Map.Strict          as Map

import           Crypto.Hash              (hash)
import qualified Data.ByteString.Char8    as BS

import           BlockChain
import           Coin
import           Keys
import           PParams
import           Slot

import           BaseTypes
import           NonIntegral              ((***))

import           Delegation.Certificates

import           Control.State.Transition

import           STS.Bhead

data VRF

instance STS VRF where
  type State VRF = (HashHeader, Slot)
  type Signal VRF = BHeader
  type Environment VRF = (Slot, PParams, Seed, PoolDistr, StakePools)
  data PredicateFailure VRF = KeyNotInPoolDistributionVRF
                          | NonceProofNotVerifiedVRF
                          | LeaderProofNotVerifiedVRF
                          | NotSlotLeaderVRF
                          | BheadFailure (PredicateFailure BHEAD)
                              deriving (Show, Eq)
  initialRules = [pure (HashHeader $ hash (BS.pack "emptyBHeader"), Slot 0)]
  transitionRules = [vrfTransition]

vrfTransition :: TransitionRule VRF
vrfTransition = do
  TRC ((sNow, pp, eta0, PoolDistr pd, stPools), (h, sL), bh) <- judgmentContext
  let bhb = bhbody bh
  let vk = bheaderVk bhb
  let ss = slotToSeed $ bheaderSlot bhb
  let f = _activeSlotCoeff pp
  let bhl = intervalValue $ bheaderL bhb
  let hkEntry = Map.lookup (hashKey vk) pd
  let Coin allStake = Map.foldr (+) (Coin 0) pd
  case hkEntry of
    Nothing -> do
      failBecause KeyNotInPoolDistributionVRF
      pure (h, sL)
    Just (Coin relStake) -> do
      let relStake' = ((fromIntegral relStake)::FixedPoint) / fromIntegral allStake
      fromRational bhl >= 1 - (1 - fromRational (intervalValue f)) *** relStake'
        ?! NotSlotLeaderVRF
      verifyVrf vk (seedOp (seedOp eta0 ss) seedEta) (bheaderPrfEta bhb)
        ?! NonceProofNotVerifiedVRF
      verifyVrf vk (seedOp (seedOp eta0 ss) seedL) (bheaderPrfL bhb)
        ?! LeaderProofNotVerifiedVRF
      (h', sL') <- trans @BHEAD $ TRC ((sNow, pp, stPools), (h, sL), bh)
      pure (h', sL')

instance Embed BHEAD VRF where
  wrapFailed = BheadFailure
