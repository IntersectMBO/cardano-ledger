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

module Shelley.Spec.Ledger.STS.Ppup
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
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys
import Cardano.Ledger.Serialization (decodeRecordSum)
import Cardano.Ledger.Slot
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Records
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.LedgerState (PPUPState (..), pvCanFollow)
import Shelley.Spec.Ledger.PParams

data PPUP era

data PPUPEnv era
  = PPUPEnv SlotNo (Core.PParams era) (GenDelegs (Crypto era))

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
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
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
    HasField "_protocolVersion" (Core.PParams era) ProtVer,
    HasField "_protocolVersion" (PParamsDelta era) (StrictMaybe ProtVer)
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
      let badPVs = Map.filter (not . goodPV) pup
      case Map.toList (Map.map (getField @"_protocolVersion") badPVs) of
        ((_, SJust pv) : _) -> failBecause $ PVCannotFollowPPUP pv
        _ -> pure ()

      sp <- liftSTS $ asks stabilityWindow
      firstSlotNextEpoch <- do
        ei <- liftSTS $ asks epochInfo
        EpochNo e <- liftSTS $ epochInfoEpoch ei slot
        let newEpochNo = EpochNo $ e + 1
        tellEvent $ NewEpoch newEpochNo
        liftSTS $ epochInfoFirst ei newEpochNo
      let tooLate = firstSlotNextEpoch *- Duration (2 * sp)

      currentEpoch <- liftSTS $ do
        ei <- asks epochInfo
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
