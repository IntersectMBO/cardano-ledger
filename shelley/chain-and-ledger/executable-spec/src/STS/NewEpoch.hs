{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.NewEpoch
  ( NEWEPOCH,
  )
where

import           BaseTypes
import           Cardano.Ledger.Shelley.Crypto
import           Coin
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition
import           Control.State.Transition.Generator
import           Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import           Delegation.Certificates
import           EpochBoundary
import           Hedgehog (Gen)
import           LedgerState
import           Slot
import           STS.Epoch
import           TxData

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
    = EpochFailure (PredicateFailure (EPOCH crypto))
    deriving (Show, Eq)

  initialRules =
    [ pure $
        NewEpochState
          (EpochNo 0)
          (BlocksMade Map.empty)
          (BlocksMade Map.empty)
          emptyEpochState
          Nothing
          (PoolDistr Map.empty)
          Map.empty
    ]

  transitionRules = [newEpochTransition]

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
      let es_ = case ru of
            Nothing -> es
            Just ru' -> applyRUpd ru' es
      es' <- trans @(EPOCH crypto) $ TRC ((), es_, e)
      let EpochState _acnt ss _ls pp = es'
          (Stake stake, delegs) = _pstakeSet ss
          Coin total = Map.foldl (+) (Coin 0) stake
          sd =
            aggregatePlus $
              catMaybes
                [ (,fromIntegral c / fromIntegral total) <$> Map.lookup hk delegs
                  | (hk, Coin c) <- Map.toList stake
                ]
          pd' = Map.intersectionWith (,) sd (Map.map _poolVrf (_poolsSS ss))
      osched' <- liftSTS $ overlaySchedule e gkeys pp
      pure $
        NewEpochState
          e
          bcur
          (BlocksMade Map.empty)
          es'
          Nothing
          (PoolDistr pd')
          osched'
  where
    aggregatePlus = Map.fromListWith (+)

instance
  Crypto crypto =>
  Embed (EPOCH crypto) (NEWEPOCH crypto)
  where
  wrapFailed = EpochFailure

instance
  Crypto crypto =>
  HasTrace (NEWEPOCH crypto)
  where

  envGen _ = undefined :: Gen (NewEpochEnv crypto)

  sigGen _ _ = undefined :: Gen EpochNo

  type BaseEnv (NEWEPOCH crypto) = Globals

  interpretSTS globals act = runIdentity $ runReaderT act globals
