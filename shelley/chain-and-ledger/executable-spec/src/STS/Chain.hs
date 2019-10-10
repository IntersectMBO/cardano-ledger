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
  , totalAda
  )
where

import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           BaseTypes
import           BlockChain
import           Coin (Coin (..))
import           Keys
import           LedgerState
import           OCert
import           Slot
import           Tx
import           UTxO (balance)

import qualified Cardano.Crypto.VRF as VRF
import           Control.State.Transition

import           STS.Bbody
import           STS.Bhead
import           STS.Overlay
import           STS.Prtcl

data CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo

data ChainState hashAlgo dsignAlgo kesAlgo vrfAlgo
  = ChainState
      (NewEpochState hashAlgo dsignAlgo vrfAlgo)
      Nonce
      Nonce
      (HashHeader hashAlgo dsignAlgo kesAlgo vrfAlgo)
      Slot
  deriving (Show, Eq)

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => STS (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  type State (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = ChainState hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Signal (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = Block hashAlgo dsignAlgo kesAlgo vrfAlgo

  type Environment (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo) = Slot

  data PredicateFailure (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
    = BbodyFailure (PredicateFailure (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo))
    | BheadFailure (PredicateFailure (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo))
    | PrtclFailure (PredicateFailure (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [chainTransition]

chainTransition
  :: forall hashAlgo dsignAlgo kesAlgo vrfAlgo
   . ( HashAlgorithm hashAlgo
     , DSIGNAlgorithm dsignAlgo
     , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
     , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
     , KESAlgorithm kesAlgo
     , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
     , VRFAlgorithm vrfAlgo
     , VRF.Signable vrfAlgo Seed
     )
  => TransitionRule (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
chainTransition = do
  TRC (sNow, ChainState nes etaV etaC h sL, block@(Block bh _)) <- judgmentContext

  let gkeys = getGKeys nes
  nes' <-
    trans @(BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo) $ TRC (BheadEnv etaC gkeys, nes, bh)

  let NewEpochState _ eta0 _ bcur es _ _pd osched = nes'
  let EpochState (AccountState _ _reserves) _ ls pp                         = es
  let LedgerState _ (DPState (DState _ _ _ _ _ _dms _) (PState _ _ _ cs)) _ = ls

  PrtclState cs' h' sL' etaV' etaC' <- trans @(PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo)
    $ TRC (PrtclEnv (OverlayEnv pp osched eta0 _pd _dms) sNow, PrtclState cs h sL etaV etaC, bh)

  let ls' = setIssueNumbers ls cs'
  BbodyState ls'' bcur' <- trans @(BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo)
    $ TRC (BbodyEnv (Map.keysSet osched) pp _reserves, BbodyState ls' bcur, block)

  let nes'' = updateNES nes' bcur' ls''

  pure $ ChainState nes'' etaV' etaC' h' sL'

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed (BBODY hashAlgo dsignAlgo kesAlgo vrfAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = BbodyFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed (BHEAD hashAlgo dsignAlgo kesAlgo vrfAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = BheadFailure

instance
  ( HashAlgorithm hashAlgo
  , DSIGNAlgorithm dsignAlgo
  , Signable dsignAlgo (VKeyES kesAlgo, Natural, KESPeriod)
  , Signable dsignAlgo (TxBody hashAlgo dsignAlgo vrfAlgo)
  , KESAlgorithm kesAlgo
  , KESignable kesAlgo (BHBody hashAlgo dsignAlgo kesAlgo vrfAlgo)
  , VRFAlgorithm vrfAlgo
  , VRF.Signable vrfAlgo Seed
  )
  => Embed (PRTCL hashAlgo dsignAlgo kesAlgo vrfAlgo) (CHAIN hashAlgo dsignAlgo kesAlgo vrfAlgo)
 where
  wrapFailed = PrtclFailure

-- |Calculate the total ada in the chain state
totalAda :: ChainState hashAlgo dsignAlgo kesAlgo vrfAlgo -> Coin
totalAda (ChainState nes _ _ _ _) =
  treasury_ + reserves_ + rewards_ + circulation + deposits + fees_
  where
    (EpochState (AccountState treasury_ reserves_) _ ls _) = nesEs nes
    (UTxOState u deposits fees_ _) = _utxoState ls
    (DPState ds _) = _delegationState ls
    rewards_ = sum (Map.elems (_rewards ds))
    circulation = balance u
