{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Rules.Ppup
  ( ShelleyPPUP,
    PpupEnv (..),
    ShelleyPpupPredFailure (..),
    PpupEvent (..),
    PredicateFailure,
    VotingPeriod (..),
  )
where

import Cardano.Ledger.BaseTypes
  ( Globals (stabilityWindow),
    ProtVer,
    ShelleyBase,
    StrictMaybe (SJust),
    epochInfoPure,
    invalidKey,
  )
import Cardano.Ledger.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeRecordSum,
    decodeWord,
    encodeListLen,
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs (GenDelegs), KeyHash, KeyRole (Genesis))
import Cardano.Ledger.Shelley.Era (ShelleyPPUP)
import Cardano.Ledger.Shelley.PParams
  ( PPUPState (..),
    ProposedPPUpdates (ProposedPPUpdates),
    Update (..),
    pvCanFollow,
  )
import Cardano.Ledger.Slot
  ( Duration (Duration),
    EpochNo (EpochNo),
    SlotNo,
    epochInfoEpoch,
    epochInfoFirst,
    (*-),
  )
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data PpupEnv era
  = PPUPEnv SlotNo (PParams era) (GenDelegs (EraCrypto era))

data VotingPeriod = VoteForThisEpoch | VoteForNextEpoch
  deriving (Show, Eq, Generic)

instance NoThunks VotingPeriod

instance ToCBOR VotingPeriod where
  toCBOR VoteForThisEpoch = toCBOR (0 :: Word8)
  toCBOR VoteForNextEpoch = toCBOR (1 :: Word8)

instance FromCBOR VotingPeriod where
  fromCBOR =
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

instance NoThunks (ShelleyPpupPredFailure era)

newtype PpupEvent era = NewEpoch EpochNo

instance
  ( Typeable era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  STS (ShelleyPPUP era)
  where
  type State (ShelleyPPUP era) = PPUPState era
  type Signal (ShelleyPPUP era) = Maybe (Update era)
  type Environment (ShelleyPPUP era) = PpupEnv era
  type BaseM (ShelleyPPUP era) = ShelleyBase
  type PredicateFailure (ShelleyPPUP era) = ShelleyPpupPredFailure era
  type Event (ShelleyPPUP era) = PpupEvent era

  initialRules = []

  transitionRules = [ppupTransitionNonEmpty]

instance
  (Era era) =>
  ToCBOR (ShelleyPpupPredFailure era)
  where
  toCBOR = \case
    (NonGenesisUpdatePPUP a b) ->
      encodeListLen 3
        <> toCBOR (0 :: Word8)
        <> toCBOR a
        <> toCBOR b
    PPUpdateWrongEpoch ce e vp ->
      encodeListLen 4 <> toCBOR (1 :: Word8) <> toCBOR ce <> toCBOR e <> toCBOR vp
    PVCannotFollowPPUP p -> encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR p

instance
  (Era era) =>
  FromCBOR (ShelleyPpupPredFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (PPUP era)" $
    \case
      0 -> do
        a <- fromCBOR
        b <- fromCBOR
        pure (3, NonGenesisUpdatePPUP a b)
      1 -> do
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure (4, PPUpdateWrongEpoch a b c)
      2 -> do
        p <- fromCBOR
        pure (2, PVCannotFollowPPUP p)
      k -> invalidKey k

ppupTransitionNonEmpty ::
  ( Typeable era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  TransitionRule (ShelleyPPUP era)
ppupTransitionNonEmpty = do
  TRC
    ( PPUPEnv slot pp (GenDelegs _genDelegs),
      PPUPState (ProposedPPUpdates pupS) (ProposedPPUpdates fpupS),
      up
      ) <-
    judgmentContext

  case up of
    Nothing -> pure $ PPUPState (ProposedPPUpdates pupS) (ProposedPPUpdates fpupS)
    Just (Update (ProposedPPUpdates pup) te) -> do
      eval (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (eval (dom pup)) (eval (dom _genDelegs))

      let goodPV =
            pvCanFollow (getField @"_protocolVersion" pp)
              . getField @"_protocolVersion"
      let badPVs = filter (not . goodPV) (Map.elems pup)
      case map (getField @"_protocolVersion") badPVs of
        -- All Nothing cases have been filtered out by 'pvCanFollow'
        SJust pv : _ -> failBecause $ PVCannotFollowPPUP pv
        _ -> pure ()

      sp <- liftSTS $ asks stabilityWindow
      firstSlotNextEpoch <- do
        ei <- liftSTS $ asks epochInfoPure
        EpochNo e <- liftSTS $ epochInfoEpoch ei slot
        let newEpochNo = EpochNo $ e + 1
        tellEvent $ NewEpoch newEpochNo
        liftSTS $ epochInfoFirst ei newEpochNo
      let tooLate = firstSlotNextEpoch *- Duration (2 * sp)

      currentEpoch <- liftSTS $ do
        ei <- asks epochInfoPure
        epochInfoEpoch ei slot

      if slot < tooLate
        then do
          currentEpoch == te ?! PPUpdateWrongEpoch currentEpoch te VoteForThisEpoch
          pure $
            PPUPState
              (ProposedPPUpdates (eval (pupS ⨃ pup)))
              (ProposedPPUpdates fpupS)
        else do
          currentEpoch + 1 == te ?! PPUpdateWrongEpoch currentEpoch te VoteForNextEpoch
          pure $
            PPUPState
              (ProposedPPUpdates pupS)
              (ProposedPPUpdates (eval (fpupS ⨃ pup)))
