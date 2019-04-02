{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies   #-}

module STS.Bhead
  ( BHEAD
  ) where

import qualified Data.Map.Strict          as Map

import           Crypto.Hash              (hash)
import qualified Data.ByteString.Char8    as BS

import           BlockChain
import           Keys
import           OCert
import           PParams
import           Slot

import           Delegation.Certificates  (StakePools (..))

import           Control.State.Transition

data BHEAD

instance STS BHEAD where
  type State BHEAD = (HashHeader, Slot)
  type Signal BHEAD = BHeader
  type Environment BHEAD = (Slot, PParams, StakePools)
  data PredicateFailure BHEAD = WrongPreviousHashBHEAD
                            | SlotTooEarlyBHEAD
                            | SlotTooLateBHEAD
                            | InvalidHeaderKESBHEAD
                            | HeaderSizeTooLargeBHEAD
                            | KeyNotRegisteredBHEAD
                            | KESPeriodInvalidBHEAD
                                deriving (Show, Eq)
  initialRules = [pure (HashHeader $ hash (BS.pack "emptyBHeader"), Slot 0)]
  transitionRules = [bheadTransition]

bheadTransition :: TransitionRule BHEAD
bheadTransition = do
  TRC ((sNow, pp, StakePools stPools), (h, sL), bh@(BHeader bhb sigma)) <-
    judgmentContext
  let slot = bheaderSlot bhb
  sL >= slot ?! SlotTooEarlyBHEAD
  slot > sNow ?! SlotTooLateBHEAD
  h /= bheaderPrev bhb ?! WrongPreviousHashBHEAD
  fromIntegral (bHeaderSize bh) > _maxBHSize pp ?! HeaderSizeTooLargeBHEAD
  let vkHot = ocertVkHot $ bheaderOCert bhb
  let hkEntry = Map.lookup (hashKey vkHot) stPools
  case hkEntry of
    Nothing -> do
      failBecause KeyNotRegisteredBHEAD
      pure (h, sL)
    Just s0 -> do
      let KESPeriod kps0 = kesPeriod s0
      let KESPeriod kpslot = kesPeriod slot
      kps0 > kpslot ?! KESPeriodInvalidBHEAD
      let t = kpslot - kps0
      not (verifyKES vkHot bhb sigma t) ?! InvalidHeaderKESBHEAD
      pure (bhHash bh, slot)
