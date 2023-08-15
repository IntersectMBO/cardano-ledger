{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Ledger.DRepDistr where

import Cardano.Ledger.BaseTypes (Anchor (..), EpochNo, StrictMaybe)
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  decode,
  encode,
  (!>),
  (<!),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (DRep (..))
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyRole (..))
import Cardano.Ledger.TreeDiff (Expr (App), ToExpr (..))
import Cardano.Ledger.UMap
import qualified Cardano.Ledger.UMap as UMap
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Identity (Identity (..))
import Data.Kind
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pulse (Pulsable (..), complete, pulse)
import Data.Typeable
import Data.VMap (VB, VMap (..), VP, foldlWithKey)
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks (..), allNoThunks)

-- =================================================================

data DRepState c = DRepState
  { drepExpiry :: !EpochNo
  , drepAnchor :: !(StrictMaybe (Anchor c))
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks (DRepState era)

instance Crypto c => NFData (DRepState c)

instance Crypto c => DecCBOR (DRepState c) where
  decCBOR =
    decode $
      RecD DRepState
        <! From
        <! From

instance Crypto c => EncCBOR (DRepState c) where
  encCBOR DRepState {..} =
    encode $
      Rec DRepState
        !> To drepExpiry
        !> To drepAnchor

instance ToExpr (DRepState era)

drepExpiryL :: Lens' (DRepState c) EpochNo
drepExpiryL = lens drepExpiry (\x y -> x {drepExpiry = y})

drepAnchorL :: Lens' (DRepState c) (StrictMaybe (Anchor c))
drepAnchorL = lens drepAnchor (\x y -> x {drepAnchor = y})

-- =================================================================
-- Algorithm for computing the DRep stake distrubution

-- | Given three inputs
--   1) Map (Credential 'Staking c) (DRep c).   The delegation map. Inside the DRepUView of the UMap 'um' from the DState.
--   2) regDreps :: Map (Credential 'DRepRole c) (DRepState c). The map of registered DReps to their state. The first part of the VState.
--   3) stakeDistr :: VMap VB VP (Credential 'Staking c) (CompactForm Coin). The aggregated state distr. The first part of the Mark SnapShot.
--   Compute the Drep distribution of stake(Coin)
--  cost is expected to be O(size of 'stakeDistr' * log (size of 'um') * log (size of 'regDreps'))
--  This is going to be expensive, so we will want to pulse it. Without pulsing, we estimate 3-5 seconds
drepDistr ::
  UMap c ->
  Map (Credential 'DRepRole c) (DRepState c) ->
  VMap VB VP (Credential 'Staking c) (CompactForm Coin) ->
  Map (DRep c) (CompactForm Coin)
drepDistr um regDreps stakeDistr = foldlWithKey (accumDRepDistr um regDreps) Map.empty stakeDistr

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
accumDRepDistr um regDreps ans stakecred c =
  case UMap.lookup stakecred (DRepUView um) of
    Nothing -> ans
    Just drep@DRepAlwaysAbstain -> Map.insertWith UMap.addCompact drep c ans
    Just drep@DRepAlwaysNoConfidence -> Map.insertWith UMap.addCompact drep c ans
    Just drep@(DRepCredential cred2) ->
      if Map.member cred2 regDreps
        then Map.insertWith UMap.addCompact drep c ans
        else ans

-- =================================================================
-- Making the algorithm Pulseable

-- | The type of a Pulser which uses 'accumDRepDistr' as its underlying function.
--   'accumDRepDistr' will be partially applied to the components of type (UMap c)
--   and (Map (Credential 'DRepRole c) (DRepState c)) when pulsing. Note that we use two type
--   equality (~) constraints to fix both the monad 'm' and the 'ans' type, to
--   the context where we will use the type as a Pulser. The type DRepPulser must
--   have 'm' and 'ans' as its last two parameters so we can make a Pulsable instance.
--   We will always use this instantiation (DRepPulser c Identity (Map (DRep c) (CompactForm Coin)))
data DRepPulser c (m :: Type -> Type) ans where
  DRepPulser ::
    (ans ~ Map (DRep c) (CompactForm Coin), m ~ Identity) =>
    !Int ->
    !(UMap c) ->
    !(Map (Credential 'DRepRole c) (DRepState c)) ->
    -- Purposely NOT strict. Initially this is obtained from the Mark SnapShot (where it is a thunk)
    -- when we first construct the DRepDistr (at the epoch boundary). We definitely don't want to incur
    -- the cost of computing it there. It will be force by the first pulse, or the first use of TICKF,
    -- and then never again.
    (VMap VB VP (Credential 'Staking c) (CompactForm Coin)) ->
    !ans ->
    DRepPulser c m ans

deriving instance Show ans => Show (DRepPulser c m ans)

instance Typeable c => NoThunks (DRepPulser c Identity (Map (DRep c) (CompactForm Coin))) where
  showTypeOf _ = "DRepPulser"
  wNoThunks ctxt (DRepPulser n um _drep balance ans) =
    allNoThunks
      [ noThunks ctxt n
      , noThunks ctxt um
      , --   , noThunks ctxt drep -- Here there be thunks
        noThunks ctxt balance
      , noThunks ctxt ans
      ]

instance Crypto c => NFData (DRepPulser c Identity (Map (DRep c) (CompactForm Coin))) where
  rnf (DRepPulser n1 um drep bal ans) = n1 `deepseq` um `deepseq` drep `deepseq` bal `deepseq` rnf ans

instance Pulsable (DRepPulser c) where
  done (DRepPulser _ _ _ m _) = VMap.null m
  current (DRepPulser _ _ _ _ ans) = ans
  pulseM ll@(DRepPulser _ _ _ balance _) | VMap.null balance = pure ll
  pulseM (DRepPulser n um regDreps balance ans) =
    let !(!steps, !balance') = VMap.splitAt n balance -- Athough it is initialized as a thunk. We want to force it with each pulse.
        ans' = foldlWithKey (accumDRepDistr um regDreps) ans steps
     in Identity (DRepPulser n um regDreps balance' ans')

-- ===================================================================================
-- The data structure we store in the Ledger State. In some Rule under the TICK rule
-- we will update this data structure each time we Tick. Hopefully at some point
-- further Ticking becomes idempotent, and the DRep stake distribution is the result.

-- | DRepDistr is what we store in the State, It is pulsed in some sub-Rule run by the TICK rule.
--   We will need lots of instances (Generic,NoThunks,NFData,Eq,EncCBOR,DecCBOR) as
--   as this type will be part of the Ledger State, and must be serialized, printed, subject to equality tests etc.
data DRepDistr c
  = DRPulsing !(DRepPulser c Identity (Map (DRep c) (CompactForm Coin)))
  | DRComplete !(Map (DRep c) (CompactForm Coin))
  deriving (Generic, NoThunks, NFData)

instance Eq (DRepDistr c) where
  DRComplete x == DRComplete y = x == y
  DRComplete x == DRPulsing y = x == complete y
  DRPulsing x == DRComplete y = complete x == y
  DRPulsing x == DRPulsing y = complete x == complete y

instance Show (DRepDistr c) where
  show (DRComplete m) = "(DRComplete " ++ show m ++ ")"
  show (DRPulsing p) = "(DRPulsing " ++ show p ++ ")"

instance Crypto c => EncCBOR (DRepDistr c) where
  encCBOR (DRComplete x) = encode (Rec DRComplete !> To x)
  encCBOR (DRPulsing x) = encode (Rec DRComplete !> To (complete x))

instance Crypto c => DecCBOR (DRepDistr c) where
  decCBOR = decode (RecD DRComplete <! From)

instance ToExpr (DRepDistr era) where
  toExpr (DRComplete x) = App "DRComplete" [toExpr x]
  toExpr (DRPulsing x) = App "DRComplete" [toExpr (complete x)]

-- ==============================================
-- Functions for pulsing DRepDistr

startDRepDistr :: Int -> UMap c -> Map (Credential 'DRepRole c) (DRepState c) -> VMap VB VP (Credential 'Staking c) (CompactForm Coin) -> DRepDistr c
startDRepDistr _ _ regDreps _ | Map.null regDreps = DRComplete Map.empty
startDRepDistr stepSize um regDreps stakeDistr = DRPulsing $ DRepPulser stepSize um regDreps stakeDistr Map.empty

pulseDRepDistr :: DRepDistr c -> DRepDistr c
pulseDRepDistr x@(DRComplete _) = x
pulseDRepDistr (DRPulsing x) = if done x2 then DRComplete (current x2) else DRPulsing x2
  where
    x2 = pulse x

completeDRepDistr :: DRepDistr c -> DRepDistr c
completeDRepDistr (DRPulsing x) = DRComplete (complete x)
completeDRepDistr x@(DRComplete _) = x

extractDRepDistr :: DRepDistr c -> Map (DRep c) (CompactForm Coin)
extractDRepDistr (DRPulsing x) = complete x
extractDRepDistr (DRComplete x) = x
