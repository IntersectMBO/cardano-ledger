{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Bhead
  ( BHEAD
  , BheadEnv (..)
  , PredicateFailure (..)
  , State
  )
where

import           Data.Set (Set)

import           BlockChain
import           Keys
import           LedgerState
import           PParams
import           Slot
import           STS.NewEpoch
import           STS.Rupd
import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition

data BHEAD crypto

data BheadEnv crypto
  = BheadEnv (Set (GenKeyHash crypto))

instance Crypto crypto
  => STS (BHEAD crypto)
 where
  type State (BHEAD crypto)
    = NewEpochState crypto
  type Signal (BHEAD crypto)
    = BHeader crypto
  type Environment (BHEAD crypto) = BheadEnv crypto
  data PredicateFailure (BHEAD crypto)
    = HeaderSizeTooLargeBHEAD
    | BlockSizeTooLargeBHEAD
    | NewEpochFailure (PredicateFailure (NEWEPOCH crypto))
    | RupdFailure (PredicateFailure (RUPD crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

bheadTransition
  :: forall crypto
   . ( Crypto crypto)
  => TransitionRule (BHEAD crypto)
bheadTransition = do
  TRC (BheadEnv gkeys, nes@(NewEpochState _ bprev _ es _ _ _), bh@(BHeader bhb _)) <-
    judgmentContext
  let slot                = bheaderSlot bhb
  let EpochState _ _ _ pp = es

  fromIntegral (bHeaderSize bh) < _maxBHSize pp ?! HeaderSizeTooLargeBHEAD
  fromIntegral (hBbsize bhb) < _maxBBSize pp ?! BlockSizeTooLargeBHEAD

  nes' <- trans @(NEWEPOCH crypto)
    $ TRC (NewEpochEnv slot gkeys, nes, epochFromSlot slot)

  ru' <- trans @(RUPD crypto) $ TRC (RupdEnv bprev es, nesRu nes', slot)
  let nes'' = nes' { nesRu = ru' }
  pure nes''

instance Crypto crypto
  => Embed (NEWEPOCH crypto) (BHEAD crypto)
 where
  wrapFailed = NewEpochFailure

instance Crypto crypto
  => Embed (RUPD crypto) (BHEAD crypto)
 where
  wrapFailed = RupdFailure
