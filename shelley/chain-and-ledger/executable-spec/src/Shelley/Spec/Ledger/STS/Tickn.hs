{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Tickn
  ( TICKN,
    TicknEnv (..),
    TicknState (..),
    TicknPredicateFailure,
    PredicateFailure,
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), encodeListLen)
import Cardano.Ledger.Era (Era)
import Cardano.Prelude (NoUnexpectedThunks)
import Control.State.Transition
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Serialization (decodeRecordNamed)

data TICKN era

data TicknEnv era = TicknEnv
  { ticknEnvPP :: PParams era,
    ticknEnvCandidateNonce :: Nonce,
    -- | Hash of the last header of the previous epoch as a nonce.
    ticknEnvHashHeaderNonce :: Nonce
  }

data TicknState = TicknState
  { ticknStateEpochNonce :: !Nonce,
    ticknStatePrevHashNonce :: !Nonce
  }
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks TicknState

instance FromCBOR TicknState where
  fromCBOR =
    decodeRecordNamed
      "TicknState"
      (const 2)
      ( TicknState
          <$> fromCBOR
          <*> fromCBOR
      )

instance ToCBOR TicknState where
  toCBOR
    ( TicknState
        ηv
        ηc
      ) =
      mconcat
        [ encodeListLen 2,
          toCBOR ηv,
          toCBOR ηc
        ]

data TicknPredicateFailure era -- No predicate failures
  deriving (Generic, Show, Eq)

instance (Era era) => NoUnexpectedThunks (TicknPredicateFailure era)

instance (Era era) => STS (TICKN era) where
  type State (TICKN era) = TicknState
  type Signal (TICKN era) = Bool -- Marker indicating whether we are in a new epoch
  type Environment (TICKN era) = TicknEnv era
  type BaseM (TICKN era) = ShelleyBase
  type PredicateFailure (TICKN era) = TicknPredicateFailure era

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

tickTransition :: TransitionRule (TICKN era)
tickTransition = do
  TRC (TicknEnv pp ηc ηph, st@(TicknState _ ηh), newEpoch) <- judgmentContext
  pure $
    if newEpoch
      then
        TicknState
          { ticknStateEpochNonce = (ηc ⭒ ηh ⭒ _extraEntropy pp),
            ticknStatePrevHashNonce = ηph
          }
      else st
