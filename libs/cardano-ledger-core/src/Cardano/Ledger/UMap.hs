{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A 'UMap' (for Unified map) represents 3 Maps with the same domain in one direction,
--   and a fourth one in the inverse direction.
--   The advantage of using 'UMap' is that 'UMap' stores all the information compactly, by exploiting the
--   the large amount of sharing in the 2 maps.
module Cardano.Ledger.UMap (
  -- * Constructing 'UMap'
  -- $UMAP
  Trip (Triple),
  tripReward,
  tripRewardActiveDelegation,
  tripDelegation,
  UMap (..),
  umInvariant,

  -- * View and its components
  -- $VIEW
  View (..),
  unView,
  unUnify,
  viewToVMap,
  rewView,
  compactRewView,
  delView,
  ptrView,
  depositView,
  domRestrictedView,
  zero,
  zeroMaybe,
  CompactForm (CompactCoin),
  toCompact,
  fromCompact,
  addCompact,
  sumCompactCoin,
  sumRewardsView,
  sumDepositView,
  compactCoinOrError,

  -- * Set and Map operations on Views
  -- $VIEWOPS
  empty,
  delete,
  delete',
  insertWith,
  insertWith',
  insert,
  insert',
  adjust,
  lookup,
  isNull,
  domain,
  range,
  (∪),
  (⨃),
  (∪+),
  unionKeyDeposits,
  (⋪),
  (⋫),
  (◁),
  member,
  notMember,
  domRestrict,
  --  * Derived functions
  --  $DFUNS
  findWithDefault,
  size,
  unify,
  RDPair (..),
  rdPairView,
)
where

import Cardano.Ledger.Binary
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData (..))
import Control.Exception (assert)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (intersectDomPLeft)
import Data.Maybe as Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Internal as SI (Set (..))
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import NoThunks.Class (NoThunks (..))
import Prelude hiding (lookup)

-- ================================================

-- A Reward-Deposit Pair, will be used to represent the reward
-- and the deposit for a given (Credential 'Staking c)
data RDPair = RDPair
  { rdReward :: {-# UNPACK #-} !(CompactForm Coin)
  , rdDeposit :: {-# UNPACK #-} !(CompactForm Coin)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

instance EncCBOR RDPair where
  encCBOR RDPair {rdReward, rdDeposit} =
    encodeListLen 2 <> encCBOR rdReward <> encCBOR rdDeposit

instance DecCBOR RDPair where
  decCBOR =
    decodeRecordNamed "RDPair" (const 2) $ RDPair <$> decCBOR <*> decCBOR

-- ===================================================================
-- UMAP

-- | a 'Trip' compactly represents the range of 4 maps with the same domain as a single triple.
--   The space compacting Trip datatype, and the pattern Triple are equivalent to:
--
-- @
-- data Trip c = Triple
--   { coinT :: !(StrictMaybe RDPair),
--     ptrT :: !(Set Ptr),
--     poolidT :: !(StrictMaybe (KeyHash 'StakePool c))
--   }
--  deriving (Show, Eq, Generic, NoThunks, NFData)
-- @
--
-- To name the constructors of 'Trip' we use the notation @Txxx@ where each @x@ is either
-- @F@ for full, i.e. the component is present, or @E@ for empty,
-- the component is not present. There are three components
-- 1) the (CompactForm Coin) as a Word64,
-- 2) the Ptr set, and
-- 3) the pool id (KeyHash 'StakePool c) . So TEEE means none of the
-- components are present, and TEEF means only the pool id is present. etc.
-- The pattern 'Triple' will correctly use the optimal constructor.
data Trip c
  = TEEE
  | TEEF !(KeyHash 'StakePool c)
  | TEFE !(Set Ptr)
  | TEFF !(Set Ptr) !(KeyHash 'StakePool c)
  | TFEE {-# UNPACK #-} !RDPair
  | TFEF {-# UNPACK #-} !RDPair !(KeyHash 'StakePool c)
  | TFFE {-# UNPACK #-} !RDPair !(Set Ptr)
  | TFFF {-# UNPACK #-} !RDPair !(Set Ptr) !(KeyHash 'StakePool c)
  deriving (Eq, Ord, Generic, NoThunks, NFData)

instance (Crypto c) => ToJSON (Trip c) where
  toJSON = object . toTripPair
  toEncoding = Aeson.pairs . mconcat . toTripPair

toTripPair :: (Aeson.KeyValue a, Crypto c) => Trip c -> [a]
toTripPair (Triple !rd !ptr !pool) =
  [ "reward" .= fmap rdReward rd
  , "deposit" .= fmap rdDeposit rd
  , "ptr" .= ptr
  , "pool" .= pool
  ]

-- | We can view all of the constructors as a Triple.
viewTrip :: Trip c -> (StrictMaybe RDPair, Set Ptr, StrictMaybe (KeyHash 'StakePool c))
viewTrip TEEE = (SNothing, Set.empty, SNothing)
viewTrip (TEEF x) = (SNothing, Set.empty, SJust x)
viewTrip (TEFE x) = (SNothing, x, SNothing)
viewTrip (TEFF x y) = (SNothing, x, SJust y)
viewTrip (TFEE x) = (SJust x, Set.empty, SNothing)
viewTrip (TFEF x y) = (SJust x, Set.empty, SJust y)
viewTrip (TFFE x y) = (SJust x, y, SNothing)
viewTrip (TFFF x y z) = (SJust x, y, SJust z)

-- | Extract a delegated Reward-Deposit Pair if it is present. We can tell that the pair
--   is present and active when Txxx has an F in the 1st position (present) and 3rd
--   position (delegated).  I.e. TFFF and TFEF
--                                ^ ^      ^ ^
--  This is equivalent to:  pattern (Triple (SJust c) _ (SJust _)) -> Just c
tripRewardActiveDelegation :: Trip c -> Maybe RDPair
tripRewardActiveDelegation =
  \case
    TFFF c _ _ -> Just c
    TFEF c _ -> Just c
    _ -> Nothing

-- | Extract the Reward-Deposit Pair if it is present. We can tell that the reward is
--   present when Txxx has an F in the first position TFFF TFFE TFEF TFEE
--                                                     ^    ^    ^    ^
--  equivalent to the pattern (Triple (SJust c) _ _) -> Just c
tripReward :: Trip c -> Maybe RDPair
tripReward =
  \case
    TFFF c _ _ -> Just c
    TFFE c _ -> Just c
    TFEF c _ -> Just c
    TFEE c -> Just c
    _ -> Nothing

-- | Extract the Delegation PoolParams, if present. We can tell that the PoolParams are
--   present when Txxx has an F in the third position TFFF TFEF TEFF TEEF
--                                                       ^    ^    ^    ^
--  equivalent to the pattern (Triple _ _ (SJust p)) -> Just p
tripDelegation :: Trip c -> Maybe (KeyHash 'StakePool c)
tripDelegation =
  \case
    TFFF _ _ p -> Just p
    TFEF _ p -> Just p
    TEFF _ p -> Just p
    TEEF p -> Just p
    _ -> Nothing

-- | A Triple can be extracted and injected into the TEEE ... TFFF constructors.
pattern Triple :: StrictMaybe RDPair -> Set Ptr -> StrictMaybe (KeyHash 'StakePool c) -> Trip c
pattern Triple a b c <-
  (viewTrip -> (a, b, c))
  where
    Triple a b c =
      case (a, b, c) of
        (SNothing, SI.Tip, SNothing) -> TEEE
        (SNothing, SI.Tip, SJust x) -> TEEF x
        (SNothing, x, SNothing) -> TEFE x
        (SNothing, x, SJust y) -> TEFF x y
        (SJust x, SI.Tip, SNothing) -> TFEE x
        (SJust x, SI.Tip, SJust y) -> TFEF x y
        (SJust x, y, SNothing) -> TFFE x y
        (SJust x, y, SJust z) -> TFFF x y z

{-# COMPLETE Triple #-}

instance Show (Trip c) where
  show (Triple a b c) = "(Triple " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")"

-- =====================================================

-- | A unified map represents 4 Maps with domain @(Credential 'Staking c)@ for
--   keys and one more in the inverse direction with @Ptr@ for keys and @(Credential 'Staking c)@ for values.
data UMap c = UMap !(Map (Credential 'Staking c) (Trip c)) !(Map Ptr (Credential 'Staking c))
  deriving (Show, Eq, Generic, NoThunks, NFData)

instance Crypto c => ToJSON (UMap c) where
  toJSON = object . toUMapPair
  toEncoding = Aeson.pairs . mconcat . toUMapPair

toUMapPair :: (Aeson.KeyValue a, Crypto c) => UMap c -> [a]
toUMapPair (UMap !m1 !m2) =
  [ "credentials" .= m1
  , "pointers" .= m2
  ]

-- | It is worthwhile stating the invariant that holds on a Unified Map
--   The 'ptrmap' and the 'ptrT' field of the 'tripmap' are inverses.
umInvariant :: Credential 'Staking c -> Ptr -> UMap c -> Bool
umInvariant stake ptr (UMap tripmap ptrmap) = forwards && backwards
  where
    forwards =
      case Map.lookup stake tripmap of
        Nothing -> all (stake /=) ptrmap
        Just (Triple _c set _d) ->
          if Set.member ptr set
            then case Map.lookup ptr ptrmap of
              Nothing -> False
              Just stake2 -> stake == stake2
            else True
    backwards =
      case Map.lookup ptr ptrmap of
        Nothing -> all (\(Triple _ set _) -> Set.notMember ptr set) tripmap
        Just cred ->
          case Map.lookup cred tripmap of
            Nothing -> False
            Just (Triple _ set _) -> Set.member ptr set

-- =====================================================

-- VIEW

-- | A 'View' lets one view a 'UMap' in three different ways
--   A view with type @(View c key value)@ can be used like a @(Map key value)@
data View c k v where
  RewardDeposits ::
    !(UMap c) ->
    View c (Credential 'Staking c) RDPair
  Delegations ::
    !(UMap c) ->
    View c (Credential 'Staking c) (KeyHash 'StakePool c)
  Ptrs ::
    !(UMap c) ->
    View c Ptr (Credential 'Staking c)

-- ==================================================
-- short hand constructors and selectors

-- | Construct a RewardDeposits View from the two maps that make up a UMap
rdPairs ::
  Map (Credential 'Staking c) (Trip c) ->
  Map Ptr (Credential 'Staking c) ->
  View c (Credential 'Staking c) RDPair
rdPairs x y = RewardDeposits (UMap x y)

-- | Construct a Delegations View from the two maps that make up a UMap
delegations ::
  Map (Credential 'Staking c) (Trip c) ->
  Map Ptr (Credential 'Staking c) ->
  View c (Credential 'Staking c) (KeyHash 'StakePool c)
delegations x y = Delegations (UMap x y)

-- | Construct a Ptrs View from the two maps that make up a UMap
ptrs ::
  Map (Credential 'Staking c) (Trip c) ->
  Map Ptr (Credential 'Staking c) ->
  View c Ptr (Credential 'Staking c)
ptrs x y = Ptrs (UMap x y)

-- | Extract the underlying 'UMap' from a 'View'
unView :: View c k v -> UMap c
unView (RewardDeposits um) = um
unView (Delegations um) = um
unView (Ptrs um) = um

-- | Materialize a real 'Map' from a 'View'
--   This is expensive, use it wisely (like maybe once per epoch boundary to make a SnapShot)
--   See also domRestrictedView, which domain restricts before computing a view.
unUnify :: View c k v -> Map k v
unUnify (RewardDeposits (UMap tripmap _)) = Map.mapMaybe tripReward tripmap
unUnify (Delegations (UMap tripmap _)) = Map.mapMaybe tripDelegation tripmap
unUnify (Ptrs (UMap _ ptrmap)) = ptrmap

-- | Materialize a real Vector Map from a 'View'
--   This is expensive, use it wisely (like maybe once per epoch boundary to make a SnapShot)
viewToVMap :: View c k v -> VMap.VMap VMap.VB VMap.VB k v
viewToVMap view =
  case view of
    RewardDeposits (UMap tripmap _) ->
      VMap.fromListN (size view) . Maybe.mapMaybe toReward . Map.toList $ tripmap
    Delegations (UMap tripmap _) ->
      VMap.fromListN (size view) . Maybe.mapMaybe toDelegation . Map.toList $ tripmap
    Ptrs (UMap _ ptrmap) -> VMap.fromMap ptrmap
  where
    toReward (key, t) = (,) key <$> tripReward t
    toDelegation (key, t) = (,) key <$> tripDelegation t

-- | Materialize the RewardDeposits Map from a 'UMap'
rewView :: UMap c -> Map.Map (Credential 'Staking c) Coin
rewView x = Map.map (fromCompact . rdReward) $ unUnify (RewardDeposits x)

compactRewView :: UMap c -> Map.Map (Credential 'Staking c) (CompactForm Coin)
compactRewView x = Map.map rdReward $ unUnify (RewardDeposits x)

-- | Materialize the Deposit  Map from a 'UMap'
depositView :: UMap c -> Map.Map (Credential 'Staking c) Coin
depositView x = Map.map (fromCompact . rdDeposit) $ unUnify (RewardDeposits x)

-- | Materialize the RDPairs Map from a 'UMap'
rdPairView :: UMap c -> Map.Map (Credential 'Staking c) RDPair
rdPairView x = unUnify (RewardDeposits x)

-- | Materialize the Delegation Map from a 'UMap'
delView :: UMap c -> Map.Map (Credential 'Staking c) (KeyHash 'StakePool c)
delView x = unUnify (Delegations x)

-- | Materialize the Ptr Map from a 'UMap'
ptrView :: UMap c -> Map.Map Ptr (Credential 'Staking c)
ptrView x = unUnify (Ptrs x)

-- | Return the materialized View of a domain restricted Umap. if 'setk' is small this should be efficient.
domRestrictedView :: Set k -> View c k v -> Map.Map k v
domRestrictedView setk (RewardDeposits (UMap tripmap _)) =
  Map.mapMaybe tripReward (Map.restrictKeys tripmap setk)
domRestrictedView setk (Delegations (UMap tripmap _)) =
  Map.mapMaybe tripDelegation (Map.restrictKeys tripmap setk)
domRestrictedView setk (Ptrs (UMap _ ptrmap)) = Map.restrictKeys ptrmap setk

-- | All 3 'Views' are 'Foldable'
instance Foldable (View c k) where
  foldMap f (RewardDeposits (UMap tmap _)) = Map.foldlWithKey accum mempty tmap
    where
      accum ans _ (Triple (SJust ccoin) _ _) = ans <> f ccoin
      accum ans _ _ = ans
  foldMap f (Delegations (UMap tmap _)) = Map.foldlWithKey accum mempty tmap
    where
      accum ans _ (Triple _ _ (SJust c)) = ans <> f c
      accum ans _ (Triple _ _ SNothing) = ans
  foldMap f (Ptrs (UMap _ ptrmap)) = foldMap f ptrmap
  foldr accum ans0 (RewardDeposits (UMap tmap _)) = Map.foldr accum2 ans0 tmap
    where
      accum2 (Triple (SJust ccoin) _ _) ans = accum ccoin ans
      accum2 _ ans = ans
  foldr accum ans0 (Delegations (UMap tmap _)) = Map.foldr accum2 ans0 tmap
    where
      accum2 (Triple _ _ (SJust c)) ans = accum c ans
      accum2 (Triple _ _ SNothing) ans = ans
  foldr accum ans (Ptrs (UMap _ ptrmap)) = Map.foldr accum ans ptrmap

  foldl' accum ans0 (RewardDeposits (UMap tmap _)) = Map.foldl' accum2 ans0 tmap
    where
      accum2 ans = maybe ans (accum ans) . tripReward
  foldl' accum ans0 (Delegations (UMap tmap _)) = Map.foldl' accum2 ans0 tmap
    where
      accum2 ans = maybe ans (accum ans) . tripDelegation
  foldl' accum ans (Ptrs (UMap _ ptrmap)) = Map.foldl' accum ans ptrmap
  length = size

-- =======================================================
-- Operations on Triple

-- | Is there no information in a Triple? If so then we can delete it from the UnifedMap
zero :: Trip c -> Bool
zero (Triple SNothing s SNothing) | Set.null s = True
zero _ = False

zeroMaybe :: Trip c -> Maybe (Trip c)
zeroMaybe t | zero t = Nothing
zeroMaybe t = Just t

-- ==============================================================
-- Basic operations on ViewMap

-- VIEWOPS

empty :: UMap c
empty = UMap Map.empty Map.empty

delete' ::
  k ->
  View c k v ->
  View c k v
delete' stakeid (RewardDeposits (UMap tripmap ptrmap)) =
  rdPairs (Map.update ok stakeid tripmap) ptrmap
  where
    ok (Triple _ ptr poolid) = zeroMaybe (Triple SNothing ptr poolid)
delete' stakeid (Delegations (UMap tripmap ptrmap)) =
  delegations (Map.update ok stakeid tripmap) ptrmap
  where
    ok (Triple c ptr _) = zeroMaybe (Triple c ptr SNothing)
delete' ptr (Ptrs (UMap tripmap ptrmap)) =
  case Map.lookup ptr ptrmap of
    Nothing -> Ptrs (UMap tripmap ptrmap)
    Just stakeid -> ptrs (Map.update ok stakeid tripmap) (Map.delete ptr ptrmap)
      where
        ok (Triple coin ptrset poolid) = zeroMaybe (Triple coin (Set.delete ptr ptrset) poolid)

delete :: k -> View c k v -> UMap c
delete k m = unView (delete' k m)

-- | Special insertion:
--
--  Keeps the value already in the ViewMap if the key 'k' is already there:
--
-- > insertWith' (\ old new -> old) k v xs
--
-- Replaces the value already in the ViewMap with 'v', if key 'k' is already there:
--
-- > insertWith' (\ old new -> new) k v xs
--
-- Replaces the value already in the ViewMap with the sum, if key 'k' is already there:
--
-- > insertWith' (\ old new -> old+new) k v xs
--
-- Ignores 'combine' if the key 'k' is NOT already in the ViewMap, and inserts 'v':
--
-- > insertWith' combine k v xs
insertWith' ::
  (v -> v -> v) ->
  k ->
  v ->
  View c k v ->
  View c k v
insertWith' comb stakeid newpair (RewardDeposits (UMap tripmap ptrmap)) =
  rdPairs (Map.alter comb2 stakeid tripmap) ptrmap
  where
    -- Here 'v' is (CompactForm Coin), but the UMap stores Word64,
    -- so there is some implict coercion going on here using the Triple pattern
    comb2 Nothing = zeroMaybe (Triple (SJust newpair) Set.empty SNothing)
    comb2 (Just (Triple (SJust oldpair) x y)) = zeroMaybe (Triple (SJust (comb oldpair newpair)) x y)
    comb2 (Just (Triple SNothing x y)) = zeroMaybe (Triple (SJust newpair) x y)
insertWith' comb stakeid newpoolid (Delegations (UMap tripmap ptrmap)) =
  delegations (Map.alter comb2 stakeid tripmap) ptrmap
  where
    comb2 Nothing = Just (Triple SNothing Set.empty (SJust newpoolid))
    comb2 (Just (Triple x y (SJust old))) = Just (Triple x y (SJust (comb old newpoolid)))
    comb2 (Just (Triple x y SNothing)) = Just (Triple x y (SJust newpoolid))
insertWith' comb ptr stake (Ptrs (UMap tripmap ptrmap)) =
  let (oldstake, newstake) =
        case Map.lookup ptr ptrmap of -- This is tricky, because we need to retract the oldstake
          Nothing -> (stake, stake) -- and to add the newstake to maintain the UMap invariant
          Just stake2 -> (stake2, comb stake2 stake)
      -- Delete old pointer from set in Triple, but also delete the whole triple if it goes to Zero.
      retract stakeid pointer m = Map.update ok stakeid m
        where
          ok (Triple c set d) = zeroMaybe (Triple c (Set.delete pointer set) d)
      -- Add the new pointer to the set in Triple
      tripmap2 = Map.update addPtr newstake (retract oldstake ptr tripmap)
        where
          addPtr (Triple a set b) = Just (Triple a (Set.insert ptr set) b)
      ptrmap2 = Map.insert ptr newstake ptrmap
   in Ptrs (UMap tripmap2 ptrmap2)

insertWith ::
  (v -> v -> v) ->
  k ->
  v ->
  View c k v ->
  UMap c
insertWith comb k v m = unView (insertWith' comb k v m)

insert' ::
  k ->
  v ->
  View c k v ->
  View c k v
insert' = insertWith' (\_old new -> new)

insert ::
  k ->
  v ->
  View c k v ->
  UMap c
insert k v m = unView (insert' k v m)

adjust :: (RDPair -> RDPair) -> k -> View c k RDPair -> UMap c
adjust f k (RewardDeposits (UMap tripmap ptrmap)) = UMap (Map.adjust g k tripmap) ptrmap
  where
    g (Triple (SJust rdp) x y) = Triple (SJust (f rdp)) x y
    g (Triple SNothing x y) = Triple SNothing x y

-- ==================================================
lookup :: k -> View c k v -> Maybe v
lookup stakeid (RewardDeposits (UMap tripmap _)) =
  Map.lookup stakeid tripmap >>= tripReward
lookup stakeid (Delegations (UMap tripmap _)) =
  Map.lookup stakeid tripmap >>= tripDelegation
lookup ptr (Ptrs (UMap _ ptrmap)) = Map.lookup ptr ptrmap

isNull :: View c k v -> Bool
isNull (RewardDeposits (UMap tripmap _)) = all (isNothing . tripReward) tripmap
isNull (Delegations (UMap tripmap _)) = all (isNothing . tripDelegation) tripmap
isNull (Ptrs (UMap _ ptrmap)) = Map.null ptrmap

domain :: View c k v -> Set k
domain (RewardDeposits (UMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans k (Triple (SJust _) _ _) = Set.insert k ans
    accum ans _ _ = ans
domain (Delegations (UMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans k (Triple _ _ (SJust _)) = Set.insert k ans
    accum ans _k (Triple _ _ SNothing) = ans
domain (Ptrs (UMap _ ptrmap)) = Map.keysSet ptrmap

range :: View c k v -> Set v
range (RewardDeposits (UMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans _ (Triple (SJust ccoin) _ _) = Set.insert ccoin ans
    accum ans _ _ = ans
range (Delegations (UMap tripmap _)) = Map.foldlWithKey' accum Set.empty tripmap
  where
    accum ans _ (Triple _ _ (SJust v)) = Set.insert v ans
    accum ans _ (Triple _ _ SNothing) = ans
range (Ptrs (UMap _tripmap ptrmap)) =
  Set.fromList (Map.elems ptrmap) -- tripmap is the inverse of ptrmap

-- =============================================================
-- evalUnified (RewardDeposits u1 ∪ singleton hk mempty)
-- evalUnified (Ptrs u2 ∪ singleton ptr hk)

-- | Union with left preference, so if k, already exists, do nothing, if it doesn't exist insert it.
(∪) ::
  View c k v ->
  (k, v) ->
  UMap c
view ∪ (k, v) = insertWith (\old _new -> old) k v view

-- ======================================
-- evalUnified  (delegations ds ⨃ singleton hk dpool) })
-- evalUnified (rewards' ⨃ wdrls_')

-- | Union with right preference, so if 'k', already exists, then old 'v' is overwritten with the new 'v'
--   Special rules apply for the RewardDeposits view, where only the 'rdReward' field of the RDPair is
--   overwritten, and the old 'rdDeposit' value persists. In this case it is an invariant that
--   the domain 'mp' is a subset of the domain of the RewardDeposits View. See the single case in
--   module Cardano.Ledger.Shelley.Rules.Delegs, in the dealing with Withdrawals's where it is used at
--   this type.
(⨃) ::
  View c k v ->
  Map k v ->
  UMap c
(RewardDeposits (UMap tripmap ptrmap)) ⨃ mp = UMap (Map.foldlWithKey' accum tripmap mp) ptrmap
  where
    accum !ansTripmap k (RDPair ccoin _) = Map.adjust overwrite k ansTripmap
      where
        overwrite (Triple (SJust (RDPair _ deposit)) a b) = Triple (SJust (RDPair ccoin deposit)) a b
        overwrite x = x
view ⨃ mp = unView $ Map.foldlWithKey' accum view mp
  where
    accum ans k v = insertWith' (\_old new -> new) k v ans

-- ==========================================
-- evalUnified (rewards dState ∪+ registeredAggregated)
-- evalUnified (rewards' ∪+ update)
-- evalUnified  (RewardDeposits u0 ∪+ refunds)

-- | Add the aggegated Coin from aggRewMap to the RewardDeposits view of the UMap
--   we assume the domain of aggRewMap is a subset of the domain of 'tripmap'
--   The other Views (other than RewardDeposits) are not reachable, since they do not have DPair as 3rd parameter
(∪+) ::
  View c k RDPair ->
  Map k (CompactForm Coin) ->
  UMap c
(RewardDeposits (UMap tripmap ptrmap)) ∪+ aggRewMap = UMap (unionHelp tripmap aggRewMap) ptrmap

addCoinToJustRewardsPartOfRDPair :: StrictMaybe RDPair -> CompactForm Coin -> StrictMaybe RDPair
addCoinToJustRewardsPartOfRDPair SNothing _ = SNothing
addCoinToJustRewardsPartOfRDPair (SJust (RDPair rew deposit)) delta =
  SJust (RDPair (addCompact rew delta) deposit)

unionHelp ::
  Ord k =>
  Map k (Trip c) ->
  Map k (CompactForm Coin) ->
  Map k (Trip c)
unionHelp tm mm =
  let f _k (Triple p1 s deposit) delta =
        Just (Triple (addCoinToJustRewardsPartOfRDPair p1 delta) s deposit)
      -- We use Map.empty below because mm is a subset of tm, we never add anything here.
      result = Map.mergeWithKey f id (const Map.empty) tm mm
   in assert (Map.valid result) result

unionKeyDeposits :: View c k RDPair -> Map k (CompactForm Coin) -> UMap c
unionKeyDeposits view coinmap = unView (Map.foldlWithKey' accum view coinmap)
  where
    accum view1 k ccoin = insertWith' combine k (RDPair (CompactCoin 0) ccoin) view1
    combine (RDPair rew dep) (RDPair _ delta) = RDPair rew (addCompact dep delta)

-- ============================================
-- evalUnified (setSingleton hk ⋪ RewardDeposits u0)
-- evalUnified (setSingleton hk ⋪ Delegations u1)

(⋪) ::
  Set k ->
  View c k v ->
  UMap c
set ⋪ view = unView (Set.foldl' (flip delete') view set)

-- ============================================
-- evalUnified (Ptrs u2 ⋫ setSingleton hk)
-- evalUnified (Delegations u1 ⋫ retired)

-- | This is slow for Delegations and RewardDeposits Views, better hope the sets are small
(⋫) ::
  View c k v ->
  Set v ->
  UMap c
Ptrs um ⋫ set = Set.foldl' removeCredStaking um set
  where
    removeCredStaking m@(UMap m2 m1) cred =
      case Map.lookup cred m2 of
        Just (Triple _ kset _) ->
          UMap (Map.update ok cred m2) (foldr (\k pset -> Map.delete k pset) m1 kset)
          where
            ok (Triple coin _ poolid) = zeroMaybe (Triple coin Set.empty poolid)
        Nothing -> m
Delegations (UMap tmap pmap) ⋫ delegset = UMap (Map.foldlWithKey' accum tmap tmap) pmap
  where
    ok (Triple c set _) = zeroMaybe (Triple c set SNothing)
    accum ans _key (Triple _ _ SNothing) = ans
    accum ans key (Triple _ _ (SJust d)) =
      if Set.member d delegset
        then Map.update ok key ans
        else ans
RewardDeposits (UMap tmap pmap) ⋫ coinset = UMap (Map.foldlWithKey' accum tmap tmap) pmap
  where
    ok (Triple _ set d) = zeroMaybe (Triple SNothing set d)
    accum ans key (Triple (SJust ccoin) _ _) =
      if Set.member ccoin coinset
        then Map.update ok key ans
        else ans
    accum ans _ _ = ans

-- =============================================

-- eval (k ∈ dom (rewards dState))
-- eval (k ∈ dom (rewards ds)))
-- eval (hk ∈ dom (rewards ds))
-- eval (hk ∉ dom (rewards ds))

member :: k -> View c k v -> Bool
member k (RewardDeposits (UMap tmap _)) =
  case Map.lookup k tmap of
    Just (Triple (SJust _) _ _) -> True
    _ -> False
member k (Delegations (UMap tmap _)) =
  case Map.lookup k tmap of
    Just (Triple _ _ (SJust _)) -> True
    _ -> False
member k (Ptrs (UMap _ pmap)) = Map.member k pmap

notMember :: k -> View c k v -> Bool
notMember k um = not (member k um)

-- =====================================================

-- eval (dom rewards' ◁ iRReserves (_irwd ds) :: RewardAccounts (Crypto era))
-- eval (dom rewards' ◁ iRTreasury (_irwd ds) :: RewardAccounts (Crypto era))

(◁) :: View c k v -> Map k u -> Map k u
(◁) = domRestrict

domRestrict :: View c k v -> Map k u -> Map k u
domRestrict (RewardDeposits (UMap tmap _)) m = intersectDomPLeft p m tmap
  where
    p _ (Triple (SJust _) _ _) = True
    p _ _ = False
domRestrict (Delegations (UMap tmap _)) m = intersectDomPLeft p m tmap
  where
    p _ (Triple _ _ (SJust _)) = True
    p _ _ = False
domRestrict (Ptrs (UMap _ pmap)) m = Map.intersection m pmap

-- ==========================

instance
  (Crypto c) =>
  EncCBOR (Trip c)
  where
  encCBOR (Triple coin ptr pool) =
    encodeListLen 3 <> encCBOR coin <> encCBOR ptr <> encCBOR pool

instance Crypto c => DecShareCBOR (Trip c) where
  type Share (Trip c) = Interns (KeyHash 'StakePool c)
  decShareCBOR is =
    decodeRecordNamed "Triple" (const 3) $
      do
        a <- decCBOR
        b <- decCBOR
        c <- decShareMonadCBOR is
        pure (Triple a b c)

instance Crypto c => EncCBOR (UMap c) where
  encCBOR (UMap tripmap ptrmap) =
    encodeListLen 2 <> encodeMap encCBOR encCBOR tripmap <> encodeMap encCBOR encCBOR ptrmap

instance Crypto c => DecShareCBOR (UMap c) where
  type
    Share (UMap c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  decSharePlusCBOR =
    StateT
      ( \(a, b) ->
          decodeRecordNamed "UMap" (const 2) $ do
            tripmap <- decodeMap (interns a <$> decCBOR) (decShareCBOR b)
            let a' = internsFromMap tripmap <> a
            ptrmap <- decodeMap decCBOR (interns a' <$> decCBOR)
            pure (UMap tripmap ptrmap, (a', b))
      )

-- ==================================================
-- derived operations

-- DFUNS

-- | Find the value associated with a key from a View, return the default if the key is not there.
findWithDefault :: a -> k -> View c k a -> a
findWithDefault d k = fromMaybe d . lookup k

-- | A View is a view, so the size of the view is NOT the same as the size of
-- the underlying triple map.
size :: View c k a -> Int
size (Ptrs (UMap _ ptrmap)) = Map.size ptrmap
size x = foldl' (\count _v -> count + 1) 0 x

-- | Create a UMap from 3 separate maps. For use in tests only.
unify ::
  Map (Credential 'Staking c) RDPair ->
  Map (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Map Ptr (Credential 'Staking c) ->
  UMap c
unify rews dels ptrss = um3
  where
    um1 = unView $ Map.foldlWithKey' (\um k v -> insert' k v um) (RewardDeposits empty) rews
    um2 = unView $ Map.foldlWithKey' (\um k v -> insert' k v um) (Delegations um1) dels
    um3 = unView $ Map.foldlWithKey' (\um k v -> insert' k v um) (Ptrs um2) ptrss

compactCoinOrError :: HasCallStack => Coin -> CompactForm Coin
compactCoinOrError c =
  case toCompact c of
    Nothing -> error $ "Invalid ADA value in staking: " <> show c
    Just compactCoin -> compactCoin

addCompact :: CompactForm Coin -> CompactForm Coin -> CompactForm Coin
addCompact (CompactCoin x) (CompactCoin y) = CompactCoin (x + y)

sumCompactCoin :: Foldable t => t (CompactForm Coin) -> CompactForm Coin
sumCompactCoin t = foldl' addCompact (CompactCoin 0) t

sumRewardsView :: View c k RDPair -> CompactForm Coin
sumRewardsView rewview = foldl' accum (CompactCoin 0) rewview
  where
    accum ans (RDPair c _) = addCompact ans c

sumDepositView :: View c k RDPair -> CompactForm Coin
sumDepositView rewview = foldl' accum (CompactCoin 0) rewview
  where
    accum ans (RDPair _ d) = addCompact ans d

-- =================================================

instance ToExpr RDPair

instance ToExpr (Trip c)

instance ToExpr (UMap c)
