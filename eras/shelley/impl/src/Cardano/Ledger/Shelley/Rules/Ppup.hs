{-# LANGUAGE BangPatterns #-}
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
  votedFuturePParams,
)
where

import Cardano.Ledger.BaseTypes (
  Globals (quorum),
  Mismatch (..),
  ProtVer,
  Relation (..),
  ShelleyBase,
  StrictMaybe (..),
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
  EpochNo (..),
  SlotNo,
  getTheSlotOfNoReturn,
 )
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import qualified Data.Foldable as F (find)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Word (Word64, Word8)
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
      !(Mismatch 'RelSubset (Set (KeyHash 'Genesis (EraCrypto era))))
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
    NonGenesisUpdatePPUP mm ->
      encodeListLen 2
        <> encCBOR (0 :: Word8)
        <> encCBOR mm
    PPUpdateWrongEpoch ce e vp ->
      encodeListLen 4 <> encCBOR (1 :: Word8) <> encCBOR ce <> encCBOR e <> encCBOR vp
    PVCannotFollowPPUP p -> encodeListLen 2 <> encCBOR (2 :: Word8) <> encCBOR p

instance Era era => DecCBOR (ShelleyPpupPredFailure era) where
  decCBOR = decodeRecordSum "ShelleyPpupPredFailure" $
    \case
      0 -> do
        mm <- decCBOR
        pure (2, NonGenesisUpdatePPUP mm)
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
      eval (dom pup ⊆ dom genDelegs)
        ?! NonGenesisUpdatePPUP
          Mismatch
            { mismatchSupplied = eval $ dom pup
            , mismatchExpected = eval $ dom genDelegs
            }

      let firstIllegalProtVerUpdate = do
            ppu <- F.find (not . hasLegalProtVerUpdate pp) pup
            -- SNothing is considered legal
            SJust newBadProtVer <- Just (ppu ^. ppuProtocolVersionL)
            Just newBadProtVer
      failOnJust firstIllegalProtVerUpdate PVCannotFollowPPUP

      (curEpochNo, tooLate, nextEpochNo) <- liftSTS $ getTheSlotOfNoReturn slot
      tellEvent $ PpupNewEpoch nextEpochNo

      if slot < tooLate
        then do
          (curEpochNo == targetEpochNo)
            ?! PPUpdateWrongEpoch curEpochNo targetEpochNo VoteForThisEpoch
          let curProposals = ProposedPPUpdates (eval (pupS ⨃ pup))
          !coreNodeQuorum <- liftSTS $ asks quorum
          pure $
            pps
              { sgsCurProposals = curProposals
              , sgsFutureProposals = ProposedPPUpdates fpupS
              , sgsFuturePParams =
                  PotentialPParamsUpdate $ votedFuturePParams curProposals pp coreNodeQuorum
              }
        else do
          (succ curEpochNo == targetEpochNo)
            ?! PPUpdateWrongEpoch curEpochNo targetEpochNo VoteForNextEpoch
          pure $
            pps
              { sgsCurProposals = ProposedPPUpdates pupS
              , sgsFutureProposals = ProposedPPUpdates (eval (fpupS ⨃ pup))
              }

type PPUPPredFailure era = EraRuleFailure "PPUP" era
{-# DEPRECATED PPUPPredFailure "In favor of `EraRuleFailure` PPUP era" #-}

-- | If at least @n@ nodes voted to change __the same__ protocol parameters to
-- __the same__ values, return the given protocol parameters updated to these
-- values. Here @n@ is the quorum needed.
votedFuturePParams ::
  forall era.
  EraPParams era =>
  ProposedPPUpdates era ->
  -- | Protocol parameters to which the change will be applied.
  PParams era ->
  -- | Quorum needed to change the protocol parameters.
  Word64 ->
  Maybe (PParams era)
votedFuturePParams (ProposedPPUpdates pppu) pp quorumN = do
  let votes =
        Map.foldr
          (\vote -> Map.insertWith (+) vote 1)
          (Map.empty :: Map.Map (PParamsUpdate era) Word64)
          pppu
      consensus = Map.filter (>= quorumN) votes
  -- NOTE that `quorumN` is a global constant, and that we require
  -- it to be strictly greater than half the number of genesis nodes.
  -- The keys in the `pup` correspond to the genesis nodes,
  -- and therefore either:
  --   1) `consensus` is empty, or
  --   2) `consensus` has exactly one element.
  [ppu] <- Just $ Map.keys consensus
  -- NOTE that `applyPPUpdates` corresponds to the union override right
  -- operation in the formal spec.
  let ppNew = applyPPUpdates pp ppu
  -- TODO: Remove this incorrect check from the code and the spec. It is incorrect because
  -- block header size is not part of the block body size, therefore this relation makes
  -- no sense. My hypothesis is that at initial design phase there was a block size that
  -- included the block header size, which later got changed to block body size. See
  -- relevant spec ticket: https://github.com/IntersectMBO/cardano-ledger/issues/4251
  guard $
    toInteger (ppNew ^. ppMaxTxSizeL) + toInteger (ppNew ^. ppMaxBHSizeL)
      < toInteger (ppNew ^. ppMaxBBSizeL)
  pure ppNew
