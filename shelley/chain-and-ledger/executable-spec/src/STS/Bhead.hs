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

import           Control.State.Transition

data BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo

data BheadEnv hashAlgo dsignAlgo kesAlgo vrfAlgo
  = BheadEnv (Set (GenKeyHash hashAlgo dsignAlgo))

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , KESAlgorithm kesAlgo
  )
  => STS (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = NewEpochState hashAlgo dsignAlgo vrfAlgo
  type Signal (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BHeader hashAlgo dsignAlgo kesAlgo vrfAlgo
  type Environment (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo) = BheadEnv hashAlgo dsignAlgo kesAlgo vrfAlgo
  data PredicateFailure (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = HeaderSizeTooLargeBHEAD
    | BlockSizeTooLargeBHEAD
    | NewEpochFailure (PredicateFailure (NEWEPOCH hashAlgo dsignAlgo vrfAlgo))
    | RupdFailure (PredicateFailure (RUPD hashAlgo dsignAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [bheadTransition]

bheadTransition
  :: forall hashAlgo dsignAlgo kesAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , VRFAlgorithm vrfAlgo
     , KESAlgorithm kesAlgo
     )
  => TransitionRule (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
bheadTransition = do
  TRC (BheadEnv gkeys, nes@(NewEpochState _ bprev _ es _ _ _), bh@(BHeader bhb _)) <-
    judgmentContext
  let slot                = bheaderSlot bhb
  let EpochState _ _ _ pp = es

  fromIntegral (bHeaderSize bh) < _maxBHSize pp ?! HeaderSizeTooLargeBHEAD
  fromIntegral (hBbsize bhb) < _maxBBSize pp ?! BlockSizeTooLargeBHEAD

  nes' <- trans @(NEWEPOCH hashAlgo dsignAlgo vrfAlgo)
    $ TRC (NewEpochEnv slot gkeys, nes, epochFromSlot slot)

  ru' <- trans @(RUPD hashAlgo dsignAlgo vrfAlgo) $ TRC (RupdEnv bprev es, nesRu nes', slot)
  let nes'' = nes' { nesRu = ru' }
  pure nes''

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , VRFAlgorithm vrfAlgo
  , KESAlgorithm kesAlgo
  )
  => Embed (NEWEPOCH hashAlgo dsignAlgo vrfAlgo) (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = NewEpochFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , KESAlgorithm kesAlgo
  , VRFAlgorithm vrfAlgo
  )
  => Embed (RUPD hashAlgo dsignAlgo vrfAlgo) (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = RupdFailure
