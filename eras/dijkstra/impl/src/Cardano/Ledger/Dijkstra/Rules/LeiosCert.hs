{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Cardano.Ledger.Slot (epochFromSlot)
import Cardano.Slotting.EpochInfo (epochInfoSlotLength)
import Cardano.Slotting.Time (slotLengthToMillisec)
import Control.DeepSeq (NFData)
import Control.Monad (void)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
import Data.Functor.Identity (runIdentity)
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

data DijkstraLeiosCertPredFailure era
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

leiosCertTransition :: Era era => TransitionRule (LEIOSCERT era)
leiosCertTransition = do
  TRC (LeiosCertEnv {..}, (), cert) <- judgmentContext
  ei <- liftSTS $ asks epochInfoPure
  let slotLengthMs = slotLengthToMillisec $ runIdentity $ epochInfoSlotLength ei lseCurrentSlotNo
      delayMs =
        3 * toInteger (unMilliseconds32 lseAnnouncementPeriodLength)
          + toInteger (unMilliseconds32 lseVotePeriodLength)
          + toInteger (unMilliseconds32 lseDiffusionPeriodLength)
      earliestSlot =
        lseAnnouncementSlotNo + SlotNo (fromInteger ((delayMs + slotLengthMs - 1) `div` slotLengthMs))
  lseCurrentSlotNo >= earliestSlot ?! LeiosCertTooEarly (Mismatch lseCurrentSlotNo earliestSlot)

  currentEpoch <- liftSTS $ epochFromSlot lseCurrentSlotNo
  announcementEpoch <- liftSTS $ epochFromSlot lseAnnouncementSlotNo
  currentEpoch
    == announcementEpoch
      ?! LeiosCertAnnouncedInPreviousEpoch (Mismatch currentEpoch announcementEpoch)

  let threshold = unboundRational lseThreshold
      fromVerificationError = \case
        MalformedCommittee n -> LeiosCertMalformedCommittee n
        MalformedSigners -> LeiosCertMalformedSigners
        SignerWithoutKey seats -> LeiosCertSignersWithoutKey (leiosSeatIndex <$> seats)
        InsufficientWeight weight -> LeiosCertInsufficientWeight (Mismatch weight threshold)
        InvalidSignature -> LeiosCertInvalidSignature
  whenFailureFree $
    void
      ( verifyLeiosCert
          lseLeiosCommittee
          threshold
          (hashToBytes (extractHash lseAnnouncedInBlockHeader))
          cert
      )
      ?!: fromVerificationError
