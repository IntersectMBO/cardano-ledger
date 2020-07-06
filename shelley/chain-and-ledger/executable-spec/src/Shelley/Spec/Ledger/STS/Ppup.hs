{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Ppup
  ( PPUP,
    PPUPEnv (..),
    PredicateFailure (..),
    VotingPeriod (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeListLen,
    decodeWord,
    encodeListLen,
    matchSize,
  )
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Core (dom, (⊆), (⨃))
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (PPUPState (..), pvCanFollow)
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Slot

data PPUP crypto

data PPUPEnv crypto
  = PPUPEnv SlotNo PParams (GenDelegs crypto)

data VotingPeriod = VoteForThisEpoch | VoteForNextEpoch
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks VotingPeriod

instance ToCBOR VotingPeriod where
  toCBOR VoteForThisEpoch = toCBOR (0 :: Word8)
  toCBOR VoteForNextEpoch = toCBOR (1 :: Word8)

instance FromCBOR VotingPeriod where
  fromCBOR =
    decodeWord >>= \case
      0 -> pure VoteForThisEpoch
      1 -> pure VoteForNextEpoch
      k -> invalidKey k

instance Typeable crypto => STS (PPUP crypto) where
  type State (PPUP crypto) = PPUPState crypto
  type Signal (PPUP crypto) = Maybe (Update crypto)
  type Environment (PPUP crypto) = PPUPEnv crypto
  type BaseM (PPUP crypto) = ShelleyBase
  data PredicateFailure (PPUP crypto)
    = NonGenesisUpdatePPUP
        !(Set (KeyHash 'Genesis crypto)) -- KeyHashes which are voting
        !(Set (KeyHash 'Genesis crypto)) -- KeyHashes which should be voting
    | PPUpdateWrongEpoch
        !EpochNo -- current epoch
        !EpochNo -- intended epoch of update
        !VotingPeriod -- voting period within the epoch
    | PVCannotFollowPPUP
        !ProtVer -- the first bad protocol version
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [ppupTransitionNonEmpty]

instance NoUnexpectedThunks (PredicateFailure (PPUP crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (PPUP crypto))
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
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (PPUP crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "NonGenesisUpdatePPUP" 3 n
        a <- fromCBOR
        b <- fromCBOR
        pure $ NonGenesisUpdatePPUP a b
      1 -> do
        matchSize "PPUpdateWrongEpoch" 4 n
        a <- fromCBOR
        b <- fromCBOR
        c <- fromCBOR
        pure $ PPUpdateWrongEpoch a b c
      2 -> do
        matchSize "PVCannotFollowPPUP" 2 n
        p <- fromCBOR
        pure $ PVCannotFollowPPUP p
      k -> invalidKey k

ppupTransitionNonEmpty :: Typeable crypto => TransitionRule (PPUP crypto)
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
      (dom pup ⊆ dom _genDelegs) ?! NonGenesisUpdatePPUP (dom pup) (dom _genDelegs)

      let goodPV = pvCanFollow (_protocolVersion pp) . _protocolVersion
      let badPVs = Map.filter (not . goodPV) pup
      case Map.toList (Map.map _protocolVersion badPVs) of
        ((_, SJust pv) : _) -> failBecause $ PVCannotFollowPPUP pv
        _ -> pure ()

      sp <- liftSTS $ asks stabilityWindow
      firstSlotNextEpoch <- liftSTS $ do
        ei <- asks epochInfo
        EpochNo e <- epochInfoEpoch ei slot
        epochInfoFirst ei (EpochNo $ e + 1)
      let tooLate = firstSlotNextEpoch *- (Duration (2 * sp))

      currentEpoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot

      if slot < tooLate
        then do
          currentEpoch == te ?! PPUpdateWrongEpoch currentEpoch te VoteForThisEpoch
          pure $
            PPUPState
              (ProposedPPUpdates (pupS ⨃ pup))
              (ProposedPPUpdates fpupS)
        else do
          currentEpoch + 1 == te ?! PPUpdateWrongEpoch currentEpoch te VoteForNextEpoch
          pure $
            PPUPState
              (ProposedPPUpdates pupS)
              (ProposedPPUpdates (fpupS ⨃ pup))
