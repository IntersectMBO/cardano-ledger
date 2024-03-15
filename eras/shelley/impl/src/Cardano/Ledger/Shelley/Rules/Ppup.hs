{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Ppup (
  ShelleyPPUP,
  PpupEnv (..),
  ShelleyPpupPredFailure (..),
  ShelleyGovState (..),
  PpupEvent (..),
  PredicateFailure,
  VotingPeriod (..),
  PPUPPredFailure,
)
where

import Cardano.Ledger.BaseTypes (
  Globals (stabilityWindow),
  ProtVer,
  ShelleyBase,
  StrictMaybe (..),
  epochInfoPure,
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  decodeRecordSum,
  decodeWord,
  encodeListLen,
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs (GenDelegs), KeyHash, KeyRole (Genesis))
import Cardano.Ledger.Shelley.Era (ShelleyEra, ShelleyPPUP)
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.PParams (
  ProposedPPUpdates (ProposedPPUpdates),
  Update (..),
  hasLegalProtVerUpdate,
 )
import Cardano.Ledger.Slot (
  Duration (Duration),
  EpochNo (..),
  SlotNo,
  epochInfoEpoch,
  epochInfoFirst,
  (*-),
 )
import Control.DeepSeq (NFData)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import qualified Data.Foldable as F (find)
import Data.Set (Set)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))

data PpupEnv era
  = PPUPEnv SlotNo (PParams era) (GenDelegs (EraCrypto era))

data VotingPeriod = VoteForThisEpoch | VoteForNextEpoch
  deriving (Show, Eq, Generic)

instance NoThunks VotingPeriod

instance NFData VotingPeriod

instance EncCBOR VotingPeriod where
  encCBOR VoteForThisEpoch = encCBOR (0 :: Word8)
  encCBOR VoteForNextEpoch = encCBOR (1 :: Word8)

instance DecCBOR VotingPeriod where
  decCBOR =
    decodeWord >>= \case
      0 -> pure VoteForThisEpoch
      1 -> pure VoteForNextEpoch
      k -> invalidKey k

data ShelleyPpupPredFailure era
  = -- | An update was proposed by a key hash that is not one of the genesis keys.
    --  The first set contains the key hashes which were a part of the update.
    --  The second set contains the key hashes of the genesis keys.
    NonGenesisUpdatePPUP
      !(Set (KeyHash 'Genesis (EraCrypto era)))
      !(Set (KeyHash 'Genesis (EraCrypto era)))
  | -- | An update was proposed for the wrong epoch.
    --  The first 'EpochNo' is the current epoch.
    --  The second 'EpochNo' is the epoch listed in the update.
    --  The last parameter indicates if the update was intended
    --  for the current or the next epoch.
    PPUpdateWrongEpoch
      !EpochNo
      !EpochNo
      !VotingPeriod
  | -- | An update was proposed which contains an invalid protocol version.
    --  New protocol versions must either increase the major
    --  number by exactly one and set the minor version to zero,
    --  or keep the major version the same and increase the minor
    --  version by exactly one.
    PVCannotFollowPPUP
      !ProtVer
  deriving (Show, Eq, Generic)

type instance EraRuleFailure "PPUP" (ShelleyEra c) = ShelleyPpupPredFailure (ShelleyEra c)

instance InjectRuleFailure "PPUP" ShelleyPpupPredFailure (ShelleyEra c)

instance NoThunks (ShelleyPpupPredFailure era)

instance NFData (ShelleyPpupPredFailure era)

newtype PpupEvent era = PpupNewEpoch EpochNo
  deriving (Generic, Eq)

instance NFData (PpupEvent era)

instance (EraPParams era, ProtVerAtMost era 8) => STS (ShelleyPPUP era) where
  type State (ShelleyPPUP era) = ShelleyGovState era
  type Signal (ShelleyPPUP era) = StrictMaybe (Update era)
  type Environment (ShelleyPPUP era) = PpupEnv era
  type BaseM (ShelleyPPUP era) = ShelleyBase
  type PredicateFailure (ShelleyPPUP era) = ShelleyPpupPredFailure era
  type Event (ShelleyPPUP era) = PpupEvent era

  initialRules = []

  transitionRules = [ppupTransitionNonEmpty]

instance Era era => EncCBOR (ShelleyPpupPredFailure era) where
  encCBOR = \case
    NonGenesisUpdatePPUP a b ->
      encodeListLen 3
        <> encCBOR (0 :: Word8)
        <> encCBOR a
        <> encCBOR b
    PPUpdateWrongEpoch ce e vp ->
      encodeListLen 4 <> encCBOR (1 :: Word8) <> encCBOR ce <> encCBOR e <> encCBOR vp
    PVCannotFollowPPUP p -> encodeListLen 2 <> encCBOR (2 :: Word8) <> encCBOR p

instance Era era => DecCBOR (ShelleyPpupPredFailure era) where
  decCBOR = decodeRecordSum "ShelleyPpupPredFailure" $
    \case
      0 -> do
        a <- decCBOR
        b <- decCBOR
        pure (3, NonGenesisUpdatePPUP a b)
      1 -> do
        a <- decCBOR
        b <- decCBOR
        c <- decCBOR
        pure (4, PPUpdateWrongEpoch a b c)
      2 -> do
        p <- decCBOR
        pure (2, PVCannotFollowPPUP p)
      k -> invalidKey k

ppupTransitionNonEmpty :: (EraPParams era, ProtVerAtMost era 8) => TransitionRule (ShelleyPPUP era)
ppupTransitionNonEmpty = do
  TRC
    ( PPUPEnv slot pp (GenDelegs genDelegs)
      , pps@( ShelleyGovState
                { sgsCurProposals = ProposedPPUpdates pupS
                , sgsFutureProposals = ProposedPPUpdates fpupS
                }
              )
      , update
      ) <-
    judgmentContext

  case update of
    SNothing -> pure pps
    SJust (Update (ProposedPPUpdates pup) targetEpochNo) -> do
      eval (dom pup ⊆ dom genDelegs) ?! NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom genDelegs))

      let firstIllegalProtVerUpdate = do
            ppu <- F.find (not . hasLegalProtVerUpdate pp) pup
            -- SNothing is considered legal
            SJust newBadProtVer <- Just (ppu ^. ppuProtocolVersionL)
            Just newBadProtVer
      failOnJust firstIllegalProtVerUpdate PVCannotFollowPPUP

      sp <- liftSTS $ asks stabilityWindow
      (currentEpochNo, firstSlotNextEpoch) <- do
        epochInfo <- liftSTS $ asks epochInfoPure
        epochNo <- liftSTS $ epochInfoEpoch epochInfo slot
        let nextEpochNo = succ epochNo
        tellEvent $ PpupNewEpoch nextEpochNo
        liftSTS $ do
          (,) epochNo <$> epochInfoFirst epochInfo nextEpochNo

      let tooLate = firstSlotNextEpoch *- Duration (2 * sp)

      if slot < tooLate
        then do
          (currentEpochNo == targetEpochNo)
            ?! PPUpdateWrongEpoch currentEpochNo targetEpochNo VoteForThisEpoch
          pure $
            pps
              { sgsCurProposals = ProposedPPUpdates (eval (pupS ⨃ pup))
              , sgsFutureProposals = ProposedPPUpdates fpupS
              }
        else do
          (succ currentEpochNo == targetEpochNo)
            ?! PPUpdateWrongEpoch currentEpochNo targetEpochNo VoteForNextEpoch
          pure $
            pps
              { sgsCurProposals = ProposedPPUpdates pupS
              , sgsFutureProposals = ProposedPPUpdates (eval (fpupS ⨃ pup))
              }

type PPUPPredFailure era = EraRuleFailure "PPUP" era
{-# DEPRECATED PPUPPredFailure "In favor of `EraRuleFailure` PPUP era" #-}
