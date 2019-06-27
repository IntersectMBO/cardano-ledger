{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module STS.Chain
  ( CHAIN
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
import           UTxO

import           Control.State.Transition

import           STS.Bbody
import           STS.Bhead
import           STS.Prtcl

data CHAIN hashAlgo dsignAlgo kesAlgo

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
  type State (CHAIN hashAlgo dsignAlgo kesAlgo)
    = ( NewEpochState hashAlgo dsignAlgo
      , Seed
      , Seed
      , HashHeader hashAlgo dsignAlgo kesAlgo
      , Slot
      )

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
  TRC (sNow, (nes, etaV, etaC, h, sL), block@(Block bh _)) <- judgmentContext

  let gkeys = getGKeys nes
  nes' <-
    trans @(BHEAD hashAlgo dsignAlgo kesAlgo) $ TRC ((etaC, gkeys), nes, bh)

  let NewEpochState _ eta0 _ bcur es _ _pd osched = nes'
  let EpochState _ _ ls pp                        = es
  let LedgerState _ (DPState (DState _ _ _ _ _ _dms) (PState _ _ _ cs)) _ = ls

  (cs', h', sL', etaV', etaC') <- trans @(PRTCL hashAlgo dsignAlgo kesAlgo)
    $ TRC (((pp, osched, eta0, _pd, _dms), sNow), (cs, h, sL, etaV, etaC), bh)

  let ls' = setIssueNumbers ls cs'
  (ls'', bcur') <- trans @(BBODY hashAlgo dsignAlgo kesAlgo)
    $ TRC ((Map.keysSet osched, pp), (ls', bcur), block)

  let nes'' = updateNES nes' bcur' ls''

  pure (nes'', etaV', etaC', h', sL')

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
