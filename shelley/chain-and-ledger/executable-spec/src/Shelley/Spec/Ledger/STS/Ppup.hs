{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Shelley.Spec.Ledger.STS.Ppup
  ( PPUP,
    PPUPEnv (..),
    PpupPredicateFailure (..),
    PredicateFailure,
    VotingPeriod (..),
    registerProtocolParametersChange,
    votedValue
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    decodeWord,
    encodeListLen,
  )
import Cardano.Ledger.Era (Crypto, Era)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, (⊆), (⨃))
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState (PPUPState (..), pvCanFollow)
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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

instance Typeable era => STS (PPUP era) where
  type State (PPUP era) = PPUPState era
  type Signal (PPUP era) = Maybe (Update era)
  type Environment (PPUP era) = PPUPEnv era
  type BaseM (PPUP era) = ShelleyBase
  type PredicateFailure (PPUP era) = PpupPredicateFailure era

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

ppupTransitionNonEmpty :: Typeable era => TransitionRule (PPUP era)
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
              (ProposedPPUpdates (eval (pupS ⨃ pup)))
              (ProposedPPUpdates fpupS)
        else do
          currentEpoch + 1 == te ?! PPUpdateWrongEpoch currentEpoch te VoteForNextEpoch
          pure $
            PPUPState
              (ProposedPPUpdates pupS)
              (ProposedPPUpdates (eval (fpupS ⨃ pup)))

-- | Update the protocol parameter updates by clearing out the proposals and
-- making the future proposals become the new proposals, provided __all of__ the
-- new proposals can follow, or otherwise reset them.
registerProtocolParametersChange
  :: PPUPState era -> PParams era -> PPUPState era
registerProtocolParametersChange ppupState pp =  PPUPState ps emptyPPPUpdates
  where
    (ProposedPPUpdates newProposals) = futureProposals ppupState
    goodPV = pvCanFollow (_protocolVersion pp) . _protocolVersion
    ps = if all goodPV newProposals
         then ProposedPPUpdates newProposals
         else emptyPPPUpdates

-- | If at least @n@ nodes voted to change __the same__ protocol parameters to
-- __the same__ values, return the given protocol parameters updated to these
-- values. Here @n@ is the quorum needed.
votedValue ::
  PPUPState era ->
  PParams era ->
  -- ^ Protocol parameters to which the change will be applied.
  Int ->
  -- ^ Quorum needed to change the protocol parameters.
  Maybe (PParams era)
votedValue PPUPState {proposals} pps quorumN =
  let incrTally vote tally = 1 + Map.findWithDefault 0 vote tally
      ProposedPPUpdates pup = proposals
      votes =
        Map.foldr
          (\vote tally -> Map.insert vote (incrTally vote tally) tally)
          (Map.empty :: Map (PParamsUpdate era) Int)
          pup
      consensus = Map.filter (>= quorumN) votes
   in case length consensus of
        -- NOTE that `quorumN` is a global constant, and that we require
        -- it to be strictly greater than half the number of genesis nodes.
        -- The keys in the `pup` correspond to the genesis nodes,
        -- and therefore either:
        --   1) `consensus` is empty, or
        --   2) `consensus` has exactly one element.
        1 -> (Just . updatePParams pps . fst . head . Map.toList) consensus
        -- NOTE that `updatePParams` corresponds to the union override right
        -- operation in the formal spec.
        _ -> Nothing
