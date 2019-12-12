{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Snap
  ( SNAP
  , SnapState (..)
  , SnapEnv (..)
  )
where

import qualified Data.Map.Strict as Map

import           Control.Monad.Trans.Reader (asks)
import           Lens.Micro ((^.))
import           BaseTypes
import           Coin
import           EpochBoundary
import           LedgerState
import           PParams hiding (d)
import           Slot
import           Updates
import           UTxO

import           Control.State.Transition

data SNAP crypto

data SnapState crypto
  = SnapState
      (SnapShots crypto)
      (UTxOState crypto)

data SnapEnv crypto
  = SnapEnv
      PParams
      (DState crypto)
      (PState crypto)

instance STS (SNAP crypto) where
  type State (SNAP crypto) = SnapState crypto
  type Signal (SNAP crypto) = EpochNo
  type Environment (SNAP crypto) = SnapEnv crypto
  type BaseM (SNAP crypto) = ShelleyBase
  data PredicateFailure (SNAP crypto)
    = FailureSNAP
    deriving (Show, Eq)

  initialRules =
    [pure $ SnapState emptySnapShots (UTxOState (UTxO Map.empty) (Coin 0) (Coin 0) emptyUpdateState)]
  transitionRules = [snapTransition]

snapTransition :: TransitionRule (SNAP crypto)
snapTransition = do
  TRC (SnapEnv pparams d p, SnapState s u, eNew) <- judgmentContext
  let pooledStake = stakeDistr (u ^. utxo) d p
  _slot <- liftSTS $ do
    ei <- asks epochInfo
    epochInfoFirst ei eNew
  let oblg = obligation pparams (d ^. stkCreds) (p ^. stPools) _slot
  let decayed = (u ^. deposited) - oblg
  pure $ SnapState
    s { _pstakeMark = pooledStake
      , _pstakeSet = s ^. pstakeMark
      , _pstakeGo = s ^. pstakeSet
      , _poolsSS = p ^. pParams
      , _feeSS = (u ^. fees) + decayed}
    u { _deposited = oblg
      , _fees = (u ^. fees) + decayed}
