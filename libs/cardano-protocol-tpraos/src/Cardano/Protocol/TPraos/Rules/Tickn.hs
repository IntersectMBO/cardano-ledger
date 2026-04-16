{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Protocol.TPraos.Rules.Tickn (
  TICKN,
  TicknEnv (..),
  TicknState (..),
  PredicateFailure,
) where

import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), encodeListLen, toPlainEncoding)
import Cardano.Ledger.Binary.Coders (Decode (..), decode, (<!))
import Cardano.Ledger.Binary.Plain (ToCBOR (..))
import Control.State.Transition
import Data.Void (Void)
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

instance DecCBOR TicknState where
  decCBOR = decode (RecD TicknState <! From <! From)
  {-# INLINE decCBOR #-}

instance ToCBOR TicknState where
  toCBOR = toPlainEncoding shelleyProtVer . encCBOR

instance EncCBOR TicknState where
  encCBOR (TicknState ηv ηc) =
    encodeListLen 2
      <> encCBOR ηv
      <> encCBOR ηc

instance STS TICKN where
  type State TICKN = TicknState
  type Signal TICKN = Bool -- Marker indicating whether we are in a new epoch
  type Environment TICKN = TicknEnv
  type BaseM TICKN = ShelleyBase
  type PredicateFailure TICKN = Void

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
