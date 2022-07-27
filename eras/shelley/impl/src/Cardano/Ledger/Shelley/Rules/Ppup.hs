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

module Cardano.Ledger.Shelley.Rules.Ppup
  ( PPUP,
    PPUPEnv (..),
    PpupPredicateFailure (..),
    PpupEvent (..),
    PredicateFailure,
    VotingPeriod (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
  )
import Cardano.Ledger.BaseTypes
  ( Globals (stabilityWindow),
    ProtVer,
    ShelleyBase,
    StrictMaybe (SJust),
    epochInfoPure,
    invalidKey,
  )
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (GenDelegs (GenDelegs), KeyHash, KeyRole (Genesis))
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
import Data.Coders (decodeRecordSum)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks (..))

data PPUP era

data PPUPEnv era
  = PPUPEnv SlotNo (PParams era) (GenDelegs (Crypto era))

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

data PpupPredicateFailure era
  = NonGenesisUpdatePPUP
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which are voting
      !(Set (KeyHash 'Genesis (Crypto era))) -- KeyHashes which should be voting
  | PPUpdateWrongEpoch
      !EpochNo -- current epoch
      !EpochNo -- intended epoch of update
      !VotingPeriod -- voting period within the epoch
  | PVCannotFollowPPUP
      !ProtVer -- the first bad protocol version
  deriving (Show, Eq, Generic)

instance NoThunks (PpupPredicateFailure era)

newtype PpupEvent era = NewEpoch EpochNo

instance
  ( Typeable era,
    HasField "_protocolVersion" (PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsUpdate era) (StrictMaybe ProtVer)
  ) =>
  STS (PPUP era)
  where
  type State (PPUP era) = PPUPState era
  type Signal (PPUP era) = Maybe (Update era)
  type Environment (PPUP era) = PPUPEnv era
  type BaseM (PPUP era) = ShelleyBase
  type PredicateFailure (PPUP era) = PpupPredicateFailure era
  type Event (PPUP era) = PpupEvent era

  initialRules = []

  transitionRules = [ppupTransitionNonEmpty]

instance
  (Typeable era, Era era) =>
  ToCBOR (PpupPredicateFailure era)
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
  FromCBOR (PpupPredicateFailure era)
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
  TransitionRule (PPUP era)
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
