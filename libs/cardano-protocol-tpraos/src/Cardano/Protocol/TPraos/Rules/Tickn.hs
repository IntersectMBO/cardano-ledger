{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Protocol.TPraos.Rules.Tickn (
  TICKN,
  TicknEnv (..),
  TicknState (..),
  TicknPredicateFailure,
  PredicateFailure,
)
where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (FromCBOR, ToCBOR)
import Cardano.Ledger.Binary.Plain (DecCBOR (..), EncCBOR (..), decodeRecordNamed, encodeListLen)
import Control.State.Transition
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data TICKN

data TicknEnv = TicknEnv
  { ticknEnvExtraEntropy :: Nonce
  , ticknEnvCandidateNonce :: Nonce
  , ticknEnvHashHeaderNonce :: Nonce
  -- ^ Hash of the last header of the previous epoch as a nonce.
  }

data TicknState = TicknState
  { ticknStateEpochNonce :: !Nonce
  , ticknStatePrevHashNonce :: !Nonce
  }
  deriving (Show, Eq, Generic)

instance NoThunks TicknState

instance FromCBOR TicknState

instance DecCBOR TicknState where
  decCBOR =
    decodeRecordNamed
      "TicknState"
      (const 2)
      ( TicknState
          <$> decCBOR
          <*> decCBOR
      )

instance ToCBOR TicknState

instance EncCBOR TicknState where
  encCBOR
    ( TicknState
        ηv
        ηc
      ) =
      mconcat
        [ encodeListLen 2
        , encCBOR ηv
        , encCBOR ηc
        ]

data TicknPredicateFailure -- No predicate failures
  deriving (Generic, Show, Eq)

instance NoThunks TicknPredicateFailure

instance STS TICKN where
  type State TICKN = TicknState
  type Signal TICKN = Bool -- Marker indicating whether we are in a new epoch
  type Environment TICKN = TicknEnv
  type BaseM TICKN = ShelleyBase
  type PredicateFailure TICKN = TicknPredicateFailure

  initialRules =
    [ pure
        ( TicknState
            initialNonce
            initialNonce
        )
    ]
    where
      initialNonce = mkNonceFromNumber 0
  transitionRules = [tickTransition]

tickTransition :: TransitionRule TICKN
tickTransition = do
  TRC (TicknEnv extraEntropy ηc ηph, st@(TicknState _ ηh), newEpoch) <- judgmentContext
  pure $
    if newEpoch
      then
        TicknState
          { ticknStateEpochNonce = ηc ⭒ ηh ⭒ extraEntropy
          , ticknStatePrevHashNonce = ηph
          }
      else st
