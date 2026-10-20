{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Dijkstra.Rules.LeiosCert (
  LEIOSCERT,
  LeiosCertEnv (..),
  DijkstraLeiosCertPredFailure (..),
  leiosCertMessage,
) where

import Cardano.Crypto.Hash.Class (hashToBytes)
import Cardano.Crypto.Leios (
  LeiosCert,
  LeiosCommittee,
  LeiosSeatId (..),
  VerificationError (..),
  verifyLeiosCert,
 )
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..))
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra.Era (DijkstraEra, LEIOSCERT)
import Cardano.Slotting.EpochInfo (epochInfoEpoch, epochInfoSlotLength)
import Cardano.Slotting.Time (getSlotLength)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.ByteString (ByteString)
import Data.Functor.Identity (runIdentity)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word16)
import GHC.Generics (Generic)

data LeiosCertEnv = LeiosCertEnv
  { lseCurrentSlotNo :: SlotNo
  , lseAnnouncementSlotNo :: SlotNo
  , lseAnnouncedInBlockHeader :: SafeHash EraIndependentBlockHeader
  , lseLeiosCommittee :: LeiosCommittee
  , lseThreshold :: UnitInterval
  , lseAnnouncementPeriodLength :: Milliseconds32
  , lseVotePeriodLength :: Milliseconds32
  , lseDiffusionPeriodLength :: Milliseconds32
  }
  deriving (Eq, Show, Generic, NFData)

-- | Ledger-owned failures, including a serialisable mirror of the cryptographic
-- verification errors. The committee-size failure is defensive: the protocol
-- parameter limits committees to a 'Word16' number of seats.
data DijkstraLeiosCertPredFailure (era :: Type)
  = LeiosCertTooEarly (Mismatch RelGTEQ SlotNo)
  | LeiosCertAnnouncedInPreviousEpoch (Mismatch RelEQ EpochNo)
  | LeiosCertMalformedCommittee Int
  | LeiosCertMalformedSigners
  | LeiosCertSignersWithoutKey (NonEmpty Word16)
  | LeiosCertInsufficientWeight (Mismatch RelGTEQ Rational)
  | LeiosCertInvalidSignature
  deriving (Eq, Ord, Show, Generic, NFData)

instance EncCBOR (DijkstraLeiosCertPredFailure era) where
  encCBOR =
    encode . \case
      LeiosCertTooEarly mm -> Sum LeiosCertTooEarly 0 !> To mm
      LeiosCertAnnouncedInPreviousEpoch mm -> Sum LeiosCertAnnouncedInPreviousEpoch 1 !> To mm
      LeiosCertMalformedCommittee n -> Sum LeiosCertMalformedCommittee 2 !> To n
      LeiosCertMalformedSigners -> Sum LeiosCertMalformedSigners 3
      LeiosCertSignersWithoutKey seats -> Sum LeiosCertSignersWithoutKey 4 !> To seats
      LeiosCertInsufficientWeight mm -> Sum LeiosCertInsufficientWeight 5 !> To mm
      LeiosCertInvalidSignature -> Sum LeiosCertInvalidSignature 6

instance Typeable era => DecCBOR (DijkstraLeiosCertPredFailure era) where
  decCBOR = decode . Summands "DijkstraLeiosCertPredFailure" $ \case
    0 -> SumD LeiosCertTooEarly <! From
    1 -> SumD LeiosCertAnnouncedInPreviousEpoch <! From
    2 -> SumD LeiosCertMalformedCommittee <! From
    3 -> SumD LeiosCertMalformedSigners
    4 -> SumD LeiosCertSignersWithoutKey <! From
    5 -> SumD LeiosCertInsufficientWeight <! From
    6 -> SumD LeiosCertInvalidSignature
    n -> Invalid n

type instance EraRuleFailure "LEIOSCERT" DijkstraEra = DijkstraLeiosCertPredFailure DijkstraEra

type instance EraRuleEvent "LEIOSCERT" DijkstraEra = VoidEraRule "LEIOSCERT" DijkstraEra

instance InjectRuleFailure "LEIOSCERT" DijkstraLeiosCertPredFailure DijkstraEra

instance Era era => STS (LEIOSCERT era) where
  type Environment (LEIOSCERT era) = LeiosCertEnv
  type State (LEIOSCERT era) = ()
  type Signal (LEIOSCERT era) = LeiosCert
  type PredicateFailure (LEIOSCERT era) = DijkstraLeiosCertPredFailure era
  type Event (LEIOSCERT era) = Void
  type BaseM (LEIOSCERT era) = ShelleyBase

  initialRules = []
  transitionRules = [leiosCertTransition]

-- | The signed message is the raw hash bytes of the announcing block's header.
leiosCertMessage :: SafeHash EraIndependentBlockHeader -> ByteString
leiosCertMessage = hashToBytes . extractHash

leiosCertTransition :: Era era => TransitionRule (LEIOSCERT era)
leiosCertTransition = do
  TRC (LeiosCertEnv {..}, (), cert) <- judgmentContext
  ei <- liftSTS $ asks epochInfoPure
  let slotLengthMs =
        1000 * toRational (getSlotLength $ runIdentity $ epochInfoSlotLength ei lseCurrentSlotNo)
      delayMs =
        3 * toInteger (unMilliseconds32 lseAnnouncementPeriodLength)
          + toInteger (unMilliseconds32 lseVotePeriodLength)
          + toInteger (unMilliseconds32 lseDiffusionPeriodLength)
      -- Keep the slot duration exact, including fractional milliseconds. Rational
      -- arithmetic uses Integers, so neither the periods nor their sum can overflow.
      delaySlots = ceiling $ fromInteger delayMs / slotLengthMs
      earliestSlot = toInteger (unSlotNo lseAnnouncementSlotNo) + delaySlots
  -- Compare as Integers so a delay near the end of SlotNo's range cannot wrap.
  toInteger (unSlotNo lseCurrentSlotNo)
    >= earliestSlot
      ?! LeiosCertTooEarly (Mismatch lseCurrentSlotNo (SlotNo $ fromInteger earliestSlot))

  let currentEpoch = runIdentity $ epochInfoEpoch ei lseCurrentSlotNo
      announcementEpoch = runIdentity $ epochInfoEpoch ei lseAnnouncementSlotNo
  announcementEpoch
    == currentEpoch
      ?! LeiosCertAnnouncedInPreviousEpoch (Mismatch announcementEpoch currentEpoch)

  let threshold = unboundRational lseThreshold
      fromVerificationError = \case
        MalformedCommittee n -> LeiosCertMalformedCommittee n
        MalformedSigners -> LeiosCertMalformedSigners
        SignerWithoutKey seats -> LeiosCertSignersWithoutKey (leiosSeatIndex <$> seats)
        InsufficientWeight weight -> LeiosCertInsufficientWeight (Mismatch weight threshold)
        InvalidSignature -> LeiosCertInvalidSignature
  whenFailureFree $
    void (verifyLeiosCert lseLeiosCommittee threshold (leiosCertMessage lseAnnouncedInBlockHeader) cert)
      ?!: fromVerificationError
