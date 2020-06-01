{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.NewEpoch
  ( NEWEPOCH,
    PredicateFailure (..),
    calculatePoolDistr,
  )
where

import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.STS.Epoch
import Shelley.Spec.Ledger.STS.Mir
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.TxData

data NEWEPOCH crypto

instance
  Crypto crypto =>
  STS (NEWEPOCH crypto)
  where
  type State (NEWEPOCH crypto) = NewEpochState crypto

  type Signal (NEWEPOCH crypto) = EpochNo

  type Environment (NEWEPOCH crypto) = NewEpochEnv crypto

  type BaseM (NEWEPOCH crypto) = ShelleyBase

  data PredicateFailure (NEWEPOCH crypto)
    = EpochFailure (PredicateFailure (EPOCH crypto)) -- Subtransition Failures
    | CorruptRewardUpdate
        !(RewardUpdate crypto) -- The reward update which violates an invariant
    | MirFailure (PredicateFailure (MIR crypto)) -- Subtransition Failures
    deriving (Show, Generic, Eq)

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          SNothing
          (PoolDistr Map.empty)
          Map.empty
    ]

  transitionRules = [newEpochTransition]

instance NoUnexpectedThunks (PredicateFailure (NEWEPOCH crypto))

newEpochTransition ::
  forall crypto.
  Crypto crypto =>
  TransitionRule (NEWEPOCH crypto)
newEpochTransition = do
  TRC
    ( NewEpochEnv _s gkeys,
      src@(NewEpochState (EpochNo eL) _ bcur es ru _pd _osched),
      e@(EpochNo e_)
      ) <-
    judgmentContext
  if e_ /= eL + 1
    then pure src
    else do
      es' <- case ru of
        SNothing -> pure es
        SJust ru' -> do
          let RewardUpdate dt dr rs_ df _ = ru'
          dt + dr + (sum rs_) + df == 0 ?! CorruptRewardUpdate ru'
          pure $ applyRUpd ru' es

      es'' <- trans @(MIR crypto) $ TRC ((), es', ())
      es''' <- trans @(EPOCH crypto) $ TRC ((), es'', e)
      let EpochState _acnt ss _ls _pr pp _ = es'''
          pd' = calculatePoolDistr (_pstakeSet ss)
      osched' <- liftSTS $ overlaySchedule e gkeys pp
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es'''
          SNothing
          pd'
          osched'

calculatePoolDistr :: SnapShot crypto -> PoolDistr crypto
calculatePoolDistr (SnapShot (Stake stake) delegs poolParams) =
  let Coin total = Map.foldl' (+) (Coin 0) stake
      sd =
        Map.fromListWith (+) $
          catMaybes
            [ (,fromIntegral c / fromIntegral (if total == 0 then 1 else total))
                <$> Map.lookup hk delegs -- TODO mgudemann total could be zero (in
                -- particular when shrinking)
              | (hk, Coin c) <- Map.toList stake
            ]
   in PoolDistr $ Map.intersectionWith (,) sd (Map.map _poolVrf poolParams)

instance
  Crypto crypto =>
  Embed (EPOCH crypto) (NEWEPOCH crypto)
  where
  wrapFailed = EpochFailure

instance
  Crypto crypto =>
  Embed (MIR crypto) (NEWEPOCH crypto)
  where
  wrapFailed = MirFailure
