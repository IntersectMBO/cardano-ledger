{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Chain
  ( CHAIN
  , ChainState (..)
  )
where

import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           BaseTypes
import           BlockChain
import           Keys
import           LedgerState
import           OCert
import           Slot
import           Tx

import           Control.State.Transition

import           STS.Bbody
import           STS.Bhead
import           STS.Overlay
import           STS.Prtcl

data CHAIN hashAlgo dsignAlgo kesAlgo

data ChainState hashAlgo dsignAlgo kesAlgo
  = ChainState
      (NewEpochState hashAlgo dsignAlgo)
      Seed
      Seed
      (HashHeader hashAlgo dsignAlgo kesAlgo)
      Slot
  deriving (Show, Eq)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => STS (CHAIN hashAlgo dsignAlgo kesAlgo)
 where
  type State (CHAIN hashAlgo dsignAlgo kesAlgo) = ChainState hashAlgo dsignAlgo kesAlgo

  type Signal (CHAIN hashAlgo dsignAlgo kesAlgo)
    = Block hashAlgo dsignAlgo kesAlgo

  type Environment (CHAIN hashAlgo dsignAlgo kesAlgo) = Slot

  data PredicateFailure (CHAIN hashAlgo dsignAlgo kesAlgo)
    = BbodyFailure (PredicateFailure (BBODY hashAlgo dsignAlgo kesAlgo))
    | BheadFailure (PredicateFailure (BHEAD hashAlgo dsignAlgo kesAlgo))
    | PrtclFailure (PredicateFailure (PRTCL hashAlgo dsignAlgo kesAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [chainTransition]

chainTransition
  :: forall hashAlgo dsignAlgo kesAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
     )
  => TransitionRule (CHAIN hashAlgo dsignAlgo kesAlgo)
chainTransition = do
  TRC (sNow, ChainState nes etaV etaC h sL, block@(Block bh _)) <- judgmentContext

  let gkeys = getGKeys nes
  nes' <-
    trans @(BHEAD hashAlgo dsignAlgo kesAlgo) $ TRC (BheadEnv etaC gkeys, nes, bh)

  let NewEpochState _ eta0 _ bcur es _ _pd osched = nes'
  let EpochState _ _ ls pp                        = es
  let LedgerState _ (DPState (DState _ _ _ _ _ _dms) (PState _ _ _ cs)) _ = ls

  PrtclState cs' h' sL' etaV' etaC' <- trans @(PRTCL hashAlgo dsignAlgo kesAlgo)
    $ TRC (PrtclEnv (OverlayEnv pp osched eta0 _pd _dms) sNow, PrtclState cs h sL etaV etaC, bh)

  let ls' = setIssueNumbers ls cs'
  BbodyState ls'' bcur' <- trans @(BBODY hashAlgo dsignAlgo kesAlgo)
    $ TRC (BbodyEnv (Map.keysSet osched) pp, BbodyState ls' bcur, block)

  let nes'' = updateNES nes' bcur' ls''

  pure $ ChainState nes'' etaV' etaC' h' sL'

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (BBODY hashAlgo dsignAlgo kesAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = BbodyFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (BHEAD hashAlgo dsignAlgo kesAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = BheadFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo)
  )
  => Embed (PRTCL hashAlgo dsignAlgo kesAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo)
 where
  wrapFailed = PrtclFailure
