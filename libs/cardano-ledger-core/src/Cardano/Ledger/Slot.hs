{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Ledger.Slot (
  SlotNo (..),
  getTheSlotOfNoReturn,
  Duration (..),
  (-*),
  (+*),
  (*-),
  EpochNo (..),
  EpochSize (..),
  EpochInfo,
  -- Block number
  BlockNo (..),
  epochFromSlot,
  epochInfoEpoch,
  epochInfoFirst,
  epochInfoSize,
) where

import Cardano.Ledger.BaseTypes (Globals (Globals, stabilityWindow), ShelleyBase, epochInfoPure)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.EpochInfo (EpochInfo)
import qualified Cardano.Slotting.EpochInfo as EI
import Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))
import Control.Monad.Reader (Reader, ask, asks)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Quiet

newtype Duration = Duration {unDuration :: Word64}
  deriving (Eq, Generic, Ord, NoThunks, Num, Integral, Real, Enum)
  deriving (Show) via Quiet Duration

instance Semigroup Duration where
  (Duration x) <> (Duration y) = Duration $ x + y

instance Monoid Duration where
  mempty = Duration 0

(-*) :: SlotNo -> SlotNo -> Duration
(SlotNo s) -* (SlotNo t) = Duration (if s > t then s - t else t - s)

(+*) :: SlotNo -> Duration -> SlotNo
(SlotNo s) +* (Duration d) = SlotNo (s + d)

-- | Subtract a duration from a slot
(*-) :: SlotNo -> Duration -> SlotNo
(SlotNo s) *- (Duration d) = SlotNo (if s > d then s - d else 0)

epochFromSlot :: SlotNo -> Reader Globals EpochNo
epochFromSlot slot = do
  ei <- asks epochInfoPure
  pure $ epochInfoEpoch ei slot

epochInfoEpoch ::
  HasCallStack =>
  EpochInfo Identity ->
  SlotNo ->
  EpochNo
epochInfoEpoch ei = runIdentity . EI.epochInfoEpoch ei

epochInfoFirst ::
  HasCallStack =>
  EpochInfo Identity ->
  EpochNo ->
  SlotNo
epochInfoFirst ei = runIdentity . EI.epochInfoFirst ei

epochInfoSize ::
  HasCallStack =>
  EpochInfo Identity ->
  EpochNo ->
  EpochSize
epochInfoSize ei = runIdentity . EI.epochInfoSize ei

-- | Figure out a slot number that is two stability windows before the end of the next
-- epoch. Together with the slot number we also return the current epoch number and the
-- next epoch number.
--
-- The reason why it is called the point of no return, is because that is the point when
-- HardForkCombinator (HFC) initiates a controlled hard fork, if there is a major protocol
-- version update that forks into a new era.
getTheSlotOfNoReturn :: HasCallStack => SlotNo -> ShelleyBase (EpochNo, SlotNo, EpochNo)
getTheSlotOfNoReturn slot = do
  globals@Globals {stabilityWindow} <- ask
  let epochInfo = epochInfoPure globals
      epochNo = epochInfoEpoch epochInfo slot
      nextEpochNo = succ epochNo
      firstSlotNextEpoch = epochInfoFirst epochInfo nextEpochNo
      pointOfNoReturn = firstSlotNextEpoch *- Duration (2 * stabilityWindow)
  pure (epochNo, pointOfNoReturn, nextEpochNo)
