{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Governance.DRepPulser (
  DRepPulsingState (..),
  DRepPulser (..),
  pulseDRepPulsingState,
  completeDRepPulsingState,
  extractDRepPulsingState,
  finishDRepPulser,
  computeDrepDistr,
  psDRepDistrG,
  dormantEpoch,
  PulsingSnapshot (..),
  psProposalsL,
  psDRepDistrL,
  psDRepStateL,
  RunConwayRatify (..),
) where

import Cardano.Ledger.BaseTypes (EpochNo (..), Globals (..))
import Cardano.Ledger.Binary (
  DecCBOR (..),
  DecShareCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.CertState (CommitteeState)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayRATIFY)
import Cardano.Ledger.Conway.Governance.Internal
import Cardano.Ledger.Conway.Governance.Procedures (GovActionState)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.DRep (DRep (..), DRepState (..))
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.PoolDistr (PoolDistr (..))
import Cardano.Ledger.UMap
import qualified Cardano.Ledger.UMap as UMap
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.State.Transition.Extended
import Data.Aeson (KeyValue, ToJSON (..), object, pairs, (.=))
import Data.Default.Class (Default (..))
import Data.Foldable (toList)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Pulse (Pulsable (..), pulse)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SS
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..), allNoThunks)

-- | A snapshot of information from the previous epoch stored inside the Pulser.
--   After the pulser completes, but before the epoch turns, this information
--   is store in the 'DRComplete' constructor of the 'DRepPulsingState'
--   These are the values at the start of the current epoch. This allows the API
--   To access these "previous" values, both during and after pulsing.
data PulsingSnapshot era = PulsingSnapshot
  { psProposals :: !(StrictSeq (GovActionState era))
  , psDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
  , psDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
  }
  deriving (Generic)

psProposalsL :: Lens' (PulsingSnapshot era) (StrictSeq (GovActionState era))
psProposalsL = lens psProposals (\x y -> x {psProposals = y})

psDRepDistrL :: Lens' (PulsingSnapshot era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
psDRepDistrL = lens psDRepDistr (\x y -> x {psDRepDistr = y})

psDRepStateL :: Lens' (PulsingSnapshot era) (Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
psDRepStateL = lens psDRepState (\x y -> x {psDRepState = y})

deriving instance EraPParams era => Eq (PulsingSnapshot era)

deriving instance EraPParams era => Show (PulsingSnapshot era)

instance EraPParams era => NFData (PulsingSnapshot era)

instance EraPParams era => NoThunks (PulsingSnapshot era)

toPulsingSnapshotsPairs :: (KeyValue e a, EraPParams era) => PulsingSnapshot era -> [a]
toPulsingSnapshotsPairs gas@(PulsingSnapshot _ _ _) =
  let (PulsingSnapshot {..}) = gas
   in [ "psProposals" .= psProposals
      , "psDRepDistr" .= psDRepDistr
      , "psDRepState" .= psDRepState
      ]

instance EraPParams era => ToJSON (PulsingSnapshot era) where
  toJSON = object . toPulsingSnapshotsPairs
  toEncoding = pairs . mconcat . toPulsingSnapshotsPairs

instance Default (PulsingSnapshot era) where
  def = PulsingSnapshot mempty def def

instance EraPParams era => EncCBOR (PulsingSnapshot era) where
  encCBOR PulsingSnapshot {..} =
    encode $
      Rec PulsingSnapshot
        !> To psProposals
        !> To psDRepDistr
        !> To psDRepState

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (PulsingSnapshot era) where
  decShareCBOR _ =
    decode $
      RecD PulsingSnapshot
        <! From
        <! From
        <! From

instance EraPParams era => DecCBOR (PulsingSnapshot era) where
  decCBOR =
    decode $
      RecD PulsingSnapshot
        <! From
        <! From
        <! From

instance EraPParams era => ToCBOR (PulsingSnapshot era) where
  toCBOR = toEraCBOR @era

instance EraPParams era => FromCBOR (PulsingSnapshot era) where
  fromCBOR = fromEraCBOR @era

-- ===============================================================================
-- Algorithm for computing the DRep stake distrubution, with and without pulsing.
-- ===============================================================================

-- | Given three inputs
--
--   1) Map (Credential 'Staking c) (DRep c). The delegation map. Inside the DRepUView of
--   the UMap 'um' from the DState.
--
--   2) regDreps :: Map (Credential 'DRepRole c) (DRepState c). The map of registered
--   DReps to their state. The first part of the VState.
--
--   3) stakeDistr :: VMap VB VP (Credential 'Staking c) (CompactForm Coin). The
--   aggregated stake distr extracted from the first component of the IncrementalStake
--   i.e. (IStake credmap _) where credmap is converted to a VMap
--
--  Compute the Drep distribution of stake(Coin)
--  cost is expected to be O(size of 'stakeDistr' * log (size of 'um') * log (size of 'regDreps'))
--  This is going to be expensive, so we will want to pulse it. Without pulsing,
--  we estimate 3-5 seconds
computeDrepDistr ::
  UMap c ->
  Map (Credential 'DRepRole c) (DRepState c) ->
  Map (Credential 'Staking c) (CompactForm Coin) ->
  Map (DRep c) (CompactForm Coin)
computeDrepDistr um regDreps stakeDistr =
  Map.foldlWithKey' (accumDRepDistr um regDreps) Map.empty stakeDistr

-- | For each 'stakecred' and coin 'c', check if that credential is delegated to some DRep.
--   If so then add that coin to the aggregated map 'ans', mapping DReps to compact Coin
--   If the DRep is a DRepCredential (rather than AwaysAbstain or AlwaysNoConfidence) then check
--   that the credential is a member of the registered DRep map ('regDreps') before adding it to 'ans'
accumDRepDistr ::
  UMap c ->
  Map (Credential 'DRepRole c) (DRepState c) ->
  Map (DRep c) (CompactForm Coin) ->
  Credential 'Staking c ->
  CompactForm Coin ->
  Map (DRep c) (CompactForm Coin)
accumDRepDistr um regDreps ans stakeCred stake@(CompactCoin compactStake) = fromMaybe ans $ do
  umElem <- Map.lookup stakeCred (umElems um)
  drep <- umElemDRep umElem
  let stakeWithRewards =
        case umElemRDPair umElem of
          Nothing -> stake
          Just rdPair
            | CompactCoin compactReward <- rdReward rdPair ->
                CompactCoin (compactReward + compactStake)
  pure $ case drep of
    DRepAlwaysAbstain -> Map.insertWith UMap.addCompact drep stakeWithRewards ans
    DRepAlwaysNoConfidence -> Map.insertWith UMap.addCompact drep stakeWithRewards ans
    DRepCredential drepCred
      | Map.member drepCred regDreps -> Map.insertWith UMap.addCompact drep stakeWithRewards ans
      | otherwise -> ans

-- | The type of a Pulser which uses 'accumDRepDistr' as its underlying function.
--   'accumDRepDistr' will be partially applied to the components of type (UMap c)
--   and (Map (Credential 'DRepRole c) (DRepState c)) when pulsing. Note that we use two type
--   equality (~) constraints to fix both the monad 'm' and the 'ans' type, to
--   the context where we will use the type as a Pulser. The type DRepPulser must
--   have 'm' and 'ans' as its last two parameters so we can make a Pulsable instance.
--   We will always use this instantiation (DRepPulser era Identity (RatifyState era))
data DRepPulser era (m :: Type -> Type) ans where
  DRepPulser ::
    forall era ans m.
    (ans ~ RatifyState era, m ~ Identity, RunConwayRatify era) =>
    { dpPulseSize :: !Int
    -- ^ How many elements of 'dpBalance' to consume each pulse.
    , dpUMap :: !(UMap (EraCrypto era))
    -- ^ Snapshot containing the mapping of stake credentials to Pools.
    , dpBalance :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
    -- ^ The object we are iterating over. Shrinks with each pulse
    , dpStakeDistr :: !(Map (Credential 'Staking (EraCrypto era)) (CompactForm Coin))
    -- ^ Snapshot of the stake distr (comes from the IncrementalStake)
    , dpStakePoolDistr :: PoolDistr (EraCrypto era)
    -- ^ Snapshot of the pool distr. Lazy on purpose: See `ssStakeMarkPoolDistr` and ADR-7
    -- for explanation.
    , dpDRepDistr :: !(Map (DRep (EraCrypto era)) (CompactForm Coin))
    -- ^ The partial result that grows with each pulse. The purpose of the pulsing.
    , dpDRepState :: !(Map (Credential 'DRepRole (EraCrypto era)) (DRepState (EraCrypto era)))
    -- ^ Snapshot of registered DRep credentials
    , dpCurrentEpoch :: !EpochNo
    -- ^ Snapshot of the Epoch this pulser will complete in.
    , dpCommitteeState :: !(CommitteeState era)
    -- ^ Snapshot of the CommitteeState
    , dpEnactState :: !(EnactState era)
    -- ^ Snapshot of the EnactState, Used to build the Env of the RATIFY rule
    , dpProposals :: !(StrictSeq (GovActionState era))
    -- ^ Snapshot of the proposals. This is the Signal for the RATIFY rule
    , dpGlobals :: !Globals
    } ->
    DRepPulser era m ans

instance EraPParams era => Eq (DRepPulser era Identity (RatifyState era)) where
  x == y = finishDRepPulser (DRPulsing x) == finishDRepPulser (DRPulsing y)

instance Pulsable (DRepPulser era) where
  done DRepPulser {dpBalance} = Map.null dpBalance

  current x@(DRepPulser {}) = snd $ finishDRepPulser (DRPulsing x)

  pulseM pulser@(DRepPulser {..})
    | Map.null dpBalance = pure pulser
    | otherwise =
        let !(!steps, !balance') = Map.splitAt dpPulseSize dpBalance
            drep' = Map.foldlWithKey' (accumDRepDistr dpUMap dpDRepState) dpDRepDistr steps
         in pure (pulser {dpBalance = balance', dpDRepDistr = drep'})

  completeM x@(DRepPulser {}) = pure (snd $ finishDRepPulser @era (DRPulsing x))

deriving instance (EraPParams era, Show ans) => Show (DRepPulser era m ans)

instance EraPParams era => NoThunks (DRepPulser era Identity (RatifyState era)) where
  showTypeOf _ = "DRepPulser"
  wNoThunks ctxt drp@(DRepPulser _ _ _ _ _ _ _ _ _ _ _ _) =
    allNoThunks
      [ noThunks ctxt (dpPulseSize drp)
      , noThunks ctxt (dpUMap drp)
      , noThunks ctxt (dpBalance drp)
      , noThunks ctxt (dpStakeDistr drp)
      , -- dpStakePoolDistr is allowed to have thunks
        noThunks ctxt (dpDRepDistr drp)
      , noThunks ctxt (dpDRepState drp)
      , noThunks ctxt (dpCurrentEpoch drp)
      , noThunks ctxt (dpCommitteeState drp)
      , noThunks ctxt (dpEnactState drp)
      , noThunks ctxt (dpProposals drp)
      , noThunks ctxt (dpGlobals drp)
      ]

instance EraPParams era => NFData (DRepPulser era Identity (RatifyState era)) where
  rnf (DRepPulser n um bal stake pool drep dstate ep cs es as gs) =
    n `deepseq`
      um `deepseq`
        bal `deepseq`
          stake `deepseq`
            pool `deepseq`
              drep `deepseq`
                dstate `deepseq`
                  ep `deepseq`
                    cs `deepseq`
                      es `deepseq`
                        as `deepseq`
                          rnf gs

class
  ( STS (ConwayRATIFY era)
  , Signal (ConwayRATIFY era) ~ RatifySignal era
  , BaseM (ConwayRATIFY era) ~ Reader Globals
  , Environment (ConwayRATIFY era) ~ RatifyEnv era
  , State (ConwayRATIFY era) ~ RatifyState era
  ) =>
  RunConwayRatify era
  where
  runConwayRatify :: Globals -> RatifyEnv era -> RatifyState era -> RatifySignal era -> RatifyState era
  runConwayRatify globals ratifyEnv ratifyState (RatifySignal ratifySig) =
    let ratifyResult =
          runReader (applySTS @(ConwayRATIFY era) (TRC (ratifyEnv, ratifyState, RatifySignal $ reorderActions ratifySig))) globals
     in case ratifyResult of
          Left ps ->
            error (unlines ("Impossible: RATIFY rule never fails, but it did:" : map show (toList ps)))
          Right ratifyState' -> ratifyState'

finishDRepPulser :: DRepPulsingState era -> (PulsingSnapshot era, RatifyState era)
finishDRepPulser (DRComplete snap ratifyState) = (snap, ratifyState)
finishDRepPulser (DRPulsing (DRepPulser {..})) =
  (PulsingSnapshot dpProposals finalDRepDistr dpDRepState, ratifyState')
  where
    !finalDRepDistr = Map.foldlWithKey' (accumDRepDistr dpUMap dpDRepState) dpDRepDistr dpBalance
    !ratifyEnv =
      RatifyEnv
        { reStakeDistr = dpStakeDistr
        , reStakePoolDistr = dpStakePoolDistr
        , reDRepDistr = finalDRepDistr
        , reDRepState = dpDRepState
        , reCurrentEpoch = dpCurrentEpoch
        , reCommitteeState = dpCommitteeState
        }
    !ratifySig = RatifySignal dpProposals
    !ratifyState =
      RatifyState
        { rsEnactState = dpEnactState
        , rsEnacted = mempty
        , rsExpired = mempty
        , rsDelayed = False
        }
    !ratifyState' = runConwayRatify dpGlobals ratifyEnv ratifyState ratifySig

-- ===========================================================
-- The State which is stored in ConwayGovState
-- ===========================================================

data DRepPulsingState era
  = DRPulsing !(DRepPulser era Identity (RatifyState era))
  | DRComplete
      !(PulsingSnapshot era)
      !(RatifyState era)
  deriving (Generic, NoThunks, NFData)

dormantEpoch :: DRepPulsingState era -> Bool
dormantEpoch (DRPulsing x) = SS.null (dpProposals x)
dormantEpoch (DRComplete b _) = SS.null (psProposals b)

-- | This is potentially an expensive getter. Make sure not to use it in the first 80% of
-- the epoch.
psDRepDistrG ::
  SimpleGetter (DRepPulsingState era) (Map (DRep (EraCrypto era)) (CompactForm Coin))
psDRepDistrG = to get
  where
    get (DRComplete x _) = psDRepDistr x
    get x = psDRepDistr . fst $ finishDRepPulser x

instance EraPParams era => Eq (DRepPulsingState era) where
  x == y = finishDRepPulser x == finishDRepPulser y

instance EraPParams era => Show (DRepPulsingState era) where
  show (DRComplete x m) = "(DRComplete " ++ show x ++ " " ++ show m ++ ")"
  show x = show (uncurry DRComplete (finishDRepPulser x))

instance EraPParams era => EncCBOR (DRepPulsingState era) where
  encCBOR (DRComplete x y) = encode (Rec DRComplete !> To x !> To y)
  encCBOR x@(DRPulsing (DRepPulser {})) = encode (Rec DRComplete !> To snap !> To ratstate)
    where
      (snap, ratstate) = finishDRepPulser x

-- TODO: Implement Sharing: https://github.com/intersectmbo/cardano-ledger/issues/3486
instance EraPParams era => DecShareCBOR (DRepPulsingState era) where
  decShareCBOR _ =
    decode $
      RecD DRComplete
        <! From
        <! From

instance EraPParams era => DecCBOR (DRepPulsingState era) where
  decCBOR = decode (RecD DRComplete <! From <! From)

-- =====================================
-- High level operations of DRepDistr

pulseDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
pulseDRepPulsingState x@(DRComplete _ _) = x
pulseDRepPulsingState (DRPulsing x@(DRepPulser {})) =
  let x2 = pulse x
   in if done x2
        then uncurry DRComplete (finishDRepPulser (DRPulsing x2))
        else DRPulsing x2

completeDRepPulsingState :: DRepPulsingState era -> DRepPulsingState era
completeDRepPulsingState x@(DRPulsing _) = uncurry DRComplete (finishDRepPulser x)
completeDRepPulsingState x@(DRComplete {}) = x

extractDRepPulsingState :: DRepPulsingState era -> RatifyState era
extractDRepPulsingState x@(DRPulsing _) = snd (finishDRepPulser x)
extractDRepPulsingState (DRComplete _ x) = x
