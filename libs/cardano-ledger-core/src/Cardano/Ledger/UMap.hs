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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | A 'UMap' (for Unified map) represents
--
-- 1. 4 Maps with the same domain in one direction, as a single `Map` and
-- 2. 1 other `Map` which is an inverse of one of the other 4 Maps.
--
-- The advantage of using `UMap` is that it stores all the information
-- compactly, by exploiting the large amount of sharing in Map #1.
--
-- As for the other Map #2, we don't expect it to have much volume.
module Cardano.Ledger.UMap (
  -- * Constructing a `UMap`
  RDPair (..),
  rdRewardCoin,
  rdDepositCoin,
  UMElem (UMElem),
  umElemRDPair,
  umElemRDActive,
  RewardDelegation (..),
  umElemDRepDelegatedReward,
  umElemDelegations,
  umElemPtrs,
  umElemSPool,
  umElemDRep,
  umElemAsTuple,
  nullUMElem,
  nullUMElemMaybe,
  UMap (..),
  umElemsL,
  empty,
  umInvariant,

  -- * StakeCredentials
  StakeCredentials (..),
  toStakeCredentials,
  domRestrictedStakeCredentials,

  -- * `UView` and its components
  UView (..),
  rewDepUView,
  ptrUView,
  sPoolUView,
  dRepUView,
  unUView,
  unUnifyToVMap,
  rdPairMap,
  rewardMap,
  compactRewardMap,
  depositMap,
  ptrMap,
  invPtrMap,
  sPoolMap,
  dRepMap,
  domRestrictedMap,
  CompactForm (CompactCoin),
  toCompact,
  fromCompact,
  addCompact,
  sumCompactCoin,
  sumRewardsUView,
  sumDepositUView,
  compactCoinOrError,
  unify,
  unUnify,

  -- * Set and Map operations on `UView`s
  nullUView,
  member,
  member',
  notMember,
  delete,
  delete',
  insertWith,
  insertWith',
  insert,
  insert',
  adjust,
  lookup,
  domain,
  range,
  (∪),
  unionL,
  (⨃),
  unionR,
  (∪+),
  unionRewAgg,
  unionKeyDeposits,
  (⋪),
  domDelete,
  (⋫),
  rngDelete,
  (◁),
  domRestrict,

  -- * Derived functions
  findWithDefault,
  size,
  domDeleteAll,
  deleteStakingCredential,
  extractStakingCredential,
)
where

import Cardano.Ledger.BaseTypes (strictMaybe)
import Cardano.Ledger.Binary
import Cardano.Ledger.Coin (Coin (..), CompactForm (CompactCoin), compactCoinOrError)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Credential (Credential (..), Ptr)
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.DRep (DRep)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Control.DeepSeq (NFData (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras as MapExtras (extract, intersectDomPLeft)
import Data.Maybe as Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Set.Internal as SI (Set (Tip))
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (NoThunks (..))
import Prelude hiding (lookup, null)

-- | A Reward-Deposit Pair
-- Used to represent the reward and the deposit for a given (Credential 'Staking c)
data RDPair = RDPair
  { rdReward :: {-# UNPACK #-} !(CompactForm Coin)
  , rdDeposit :: {-# UNPACK #-} !(CompactForm Coin)
  }
  deriving (Show, Eq, Ord, Generic, NoThunks, NFData)

-- rdReward and rdDeposit return a (CompactForm Coin), These return a Coin.

rdRewardCoin :: RDPair -> Coin
rdRewardCoin rdp = fromCompact (rdReward rdp)

rdDepositCoin :: RDPair -> Coin
rdDepositCoin rdp = fromCompact (rdDeposit rdp)

instance EncCBOR RDPair where
  encCBOR RDPair {rdReward, rdDeposit} =
    encodeListLen 2 <> encCBOR rdReward <> encCBOR rdDeposit

instance DecCBOR RDPair where
  decCBOR =
    decodeRecordNamed "RDPair" (const 2) $ RDPair <$> decCBOR <*> decCBOR

-- | A `UMElem` compactly represents the range of 4 `Map`s with the same domain
-- as a single n-tuple.
--
-- This space-compacting datatype, and the pattern `UMElem` are equivalent to:
-- @
-- data Elem c = Elem
--   { rdPairT :: !(StrictMaybe RDPair),
--     ptrT :: !(Set Ptr),
--     sPoolT :: !(StrictMaybe (KeyHash 'StakePool c)), -- the stake pool identity
--     dRepT :: !(StrictMaybe (DRep c)),
--   }
--   deriving (Show, Eq, Generic, NoThunks, NFData)
-- @
--
-- To name the constructors of `UMElem` we use the notation @Txxx@ where
-- each @x@ is
-- either @F@ for full, i.e. the component is present,
-- or @E@ for empty, i.e. the component is not present.
--
-- There are four components:
-- 1) the reward-deposit pair as an `RDPair` (CompactForm Coin) (CompactForm Coin) as a pair of Word64s, the first @x@,
-- 2) the set of pointers, the second @x@,
-- 3) the stake pool id (KeyHash 'StakePool c), the third @x@, and
-- 4) the voting delegatee id (DRep c), the fourth @x@.
--
-- So,
-- TEEEE means none of the components are present,
-- TFEEE means only the reward-deposit pair is present,
-- TEFEE means only the set of pointers is present,
-- TEEFE means only the stake pool id is present. etc.
-- TEEEF means only the voting delegatee id is present, and
--
-- The pattern 'UMElem' will correctly use the optimal constructor.
data UMElem c
  = TEEEE
  | TEEEF !(DRep c)
  | TEEFE !(KeyHash 'StakePool c)
  | TEEFF !(KeyHash 'StakePool c) !(DRep c)
  | TEFEE !(Set Ptr)
  | TEFEF !(Set Ptr) !(DRep c)
  | TEFFE !(Set Ptr) !(KeyHash 'StakePool c)
  | TEFFF !(Set Ptr) !(KeyHash 'StakePool c) !(DRep c)
  | TFEEE {-# UNPACK #-} !RDPair
  | TFEEF {-# UNPACK #-} !RDPair !(DRep c)
  | TFEFE {-# UNPACK #-} !RDPair !(KeyHash 'StakePool c)
  | TFEFF {-# UNPACK #-} !RDPair !(KeyHash 'StakePool c) !(DRep c)
  | TFFEE {-# UNPACK #-} !RDPair !(Set Ptr)
  | TFFEF {-# UNPACK #-} !RDPair !(Set Ptr) !(DRep c)
  | TFFFE {-# UNPACK #-} !RDPair !(Set Ptr) !(KeyHash 'StakePool c)
  | TFFFF {-# UNPACK #-} !RDPair !(Set Ptr) !(KeyHash 'StakePool c) !(DRep c)
  deriving (Eq, Ord, Show, Generic, NoThunks, NFData)

instance Crypto c => ToJSON (UMElem c) where
  toJSON = object . toUMElemair
  toEncoding = Aeson.pairs . mconcat . toUMElemair

toUMElemair :: (Aeson.KeyValue e a, Crypto c) => UMElem c -> [a]
toUMElemair (UMElem !rd !ptr !spool !drep) =
  [ "reward" .= fmap rdReward rd
  , "deposit" .= fmap rdDeposit rd
  , "ptr" .= ptr
  , "spool" .= spool
  , "drep" .= drep
  ]

instance Crypto c => EncCBOR (UMElem c) where
  encCBOR (UMElem rd ptrSet sPool dRep) =
    encodeListLen 4 <> encCBOR rd <> encCBOR ptrSet <> encCBOR sPool <> encCBOR dRep

instance Crypto c => DecShareCBOR (UMElem c) where
  type Share (UMElem c) = Interns (KeyHash 'StakePool c)
  decShareCBOR is =
    decodeRecordNamed "UMElem" (const 4) $
      UMElem
        <$> decCBOR
        <*> ifDecoderVersionAtLeast (natVersion @9) (mempty <$ dropCBOR (Proxy @(Set Ptr))) decCBOR
        <*> decShareMonadCBOR is
        <*> decCBOR

-- | A n-Tuple view of the `UMElem`.
-- We can view all of the constructors as an `UMElem`.
umElemAsTuple ::
  UMElem c ->
  (StrictMaybe RDPair, Set Ptr, StrictMaybe (KeyHash 'StakePool c), StrictMaybe (DRep c))
umElemAsTuple = \case
  TEEEE -> (SNothing, Set.empty, SNothing, SNothing)
  TEEEF v -> (SNothing, Set.empty, SNothing, SJust v)
  TEEFE s -> (SNothing, Set.empty, SJust s, SNothing)
  TEEFF s v -> (SNothing, Set.empty, SJust s, SJust v)
  TEFEE p -> (SNothing, p, SNothing, SNothing)
  TEFEF p v -> (SNothing, p, SNothing, SJust v)
  TEFFE p s -> (SNothing, p, SJust s, SNothing)
  TEFFF p s v -> (SNothing, p, SJust s, SJust v)
  TFEEE r -> (SJust r, Set.empty, SNothing, SNothing)
  TFEEF r v -> (SJust r, Set.empty, SNothing, SJust v)
  TFEFE r s -> (SJust r, Set.empty, SJust s, SNothing)
  TFEFF r s v -> (SJust r, Set.empty, SJust s, SJust v)
  TFFEE r p -> (SJust r, p, SNothing, SNothing)
  TFFEF r p v -> (SJust r, p, SNothing, SJust v)
  TFFFE r p s -> (SJust r, p, SJust s, SNothing)
  TFFFF r p s v -> (SJust r, p, SJust s, SJust v)
{-# INLINE umElemAsTuple #-}

-- | Extract a DRep delegated reward if it is present.
-- We can tell that the pair is present and active when Txxxx has
-- an F in the 1st position (present) and 4rd position (DRep delegated).
--
-- This is equivalent to the pattern (UMElem (SJust r) _ _ (SJust d)) -> Just (r, d)
umElemDRepDelegatedReward :: UMElem c -> Maybe (CompactForm Coin, DRep c)
umElemDRepDelegatedReward = \case
  TFEEF RDPair {rdReward} dRep -> Just (rdReward, dRep)
  TFEFF RDPair {rdReward} _ dRep -> Just (rdReward, dRep)
  TFFEF RDPair {rdReward} _ dRep -> Just (rdReward, dRep)
  TFFFF RDPair {rdReward} _ _ dRep -> Just (rdReward, dRep)
  _ -> Nothing
{-# INLINE umElemDRepDelegatedReward #-}

-- | Extract a delegated reward-deposit pair if it is present.
-- We can tell that the pair is present and active when Txxxx has
-- an F in the 1st position (present) and 3rd position (delegated).
--
-- This is equivalent to the pattern (UMElem (SJust r) _ (SJust _) _) -> Just r
umElemRDActive :: UMElem c -> Maybe RDPair
umElemRDActive = \case
  TFEFE rdA _ -> Just rdA
  TFEFF rdA _ _ -> Just rdA
  TFFFE rdA _ _ -> Just rdA
  TFFFF rdA _ _ _ -> Just rdA
  _ -> Nothing
{-# INLINE umElemRDActive #-}

data RewardDelegation c
  = RewardDelegationSPO !(KeyHash 'StakePool c) !(CompactForm Coin)
  | RewardDelegationDRep !(DRep c) !(CompactForm Coin)
  | RewardDelegationBoth !(KeyHash 'StakePool c) !(DRep c) !(CompactForm Coin)

-- | Extract rewards that are either delegated to a DRep or an SPO (or both).
-- We can tell that the pair is present and active when Txxxx has F's in the 1st
-- and either 3rd or 4th or both positions. If there are no rewards or deposits
-- but the delegations still exist, then we return zero coin as reward.
umElemDelegations :: UMElem c -> Maybe (RewardDelegation c)
umElemDelegations (UMElem r _p s d) =
  let reward = strictMaybe mempty rdReward r
   in case (s, d) of
        (SJust spo, SJust drep) -> Just $ RewardDelegationBoth spo drep reward
        (SJust spo, SNothing) -> Just $ RewardDelegationSPO spo reward
        (SNothing, SJust drep) -> Just $ RewardDelegationDRep drep reward
        (SNothing, SNothing) -> Nothing
{-# INLINE umElemDelegations #-}

-- | Extract the reward-deposit pair if it is present.
-- We can tell that the reward is present when Txxxx has an F in the first position
--
-- This is equivalent to the pattern (UMElem (SJust r) _ _ _) -> Just r
umElemRDPair :: UMElem c -> Maybe RDPair
umElemRDPair = \case
  TFEEE r -> Just r
  TFEEF r _ -> Just r
  TFEFE r _ -> Just r
  TFEFF r _ _ -> Just r
  TFFEE r _ -> Just r
  TFFEF r _ _ -> Just r
  TFFFE r _ _ -> Just r
  TFFFF r _ _ _ -> Just r
  _ -> Nothing
{-# INLINE umElemRDPair #-}

-- | Extract the set of pointers if it is non-empty.
-- We can tell that the reward is present when Txxxx has an F in the second position
--
-- This is equivalent to the pattern (UMElem _ p _ _) -> Just p
umElemPtrs :: UMElem c -> Maybe (Set.Set Ptr)
umElemPtrs = \case
  TEFEE p | not (Set.null p) -> Just p
  TEFEF p _ | not (Set.null p) -> Just p
  TEFFE p _ | not (Set.null p) -> Just p
  TEFFF p _ _ | not (Set.null p) -> Just p
  TFFEE _ p | not (Set.null p) -> Just p
  TFFEF _ p _ | not (Set.null p) -> Just p
  TFFFE _ p _ | not (Set.null p) -> Just p
  TFFFF _ p _ _ | not (Set.null p) -> Just p
  _ -> Nothing
{-# INLINE umElemPtrs #-}

-- | Extract the stake delegatee pool id, if present.
-- We can tell that the pool id is present when Txxxx has an F in the third position
--
-- This is equivalent to the pattern (UMElem _ _ (SJust s) _) -> Just s
umElemSPool :: UMElem c -> Maybe (KeyHash 'StakePool c)
umElemSPool = \case
  TEEFE s -> Just s
  TEEFF s _ -> Just s
  TEFFE _ s -> Just s
  TEFFF _ s _ -> Just s
  TFEFE _ s -> Just s
  TFEFF _ s _ -> Just s
  TFFFE _ _ s -> Just s
  TFFFF _ _ s _ -> Just s
  _ -> Nothing
{-# INLINE umElemSPool #-}

-- | Extract the voting delegatee id, if present.
-- We can tell that the delegatee is present when Txxxx has an F in the fourth position
--
-- This is equivalent to the pattern (UMElem _ _ _ (SJust d)) -> Just d
umElemDRep :: UMElem c -> Maybe (DRep c)
umElemDRep = \case
  TEEEF d -> Just d
  TEEFF _ d -> Just d
  TEFEF _ d -> Just d
  TEFFF _ _ d -> Just d
  TFEEF _ d -> Just d
  TFEFF _ _ d -> Just d
  TFFEF _ _ d -> Just d
  TFFFF _ _ _ d -> Just d
  _ -> Nothing
{-# INLINE umElemDRep #-}

-- | A `UMElem` can be extracted and injected into the `TEEEE` ... `TFFFF` constructors.
pattern UMElem ::
  StrictMaybe RDPair ->
  Set Ptr ->
  StrictMaybe (KeyHash 'StakePool c) ->
  StrictMaybe (DRep c) ->
  UMElem c
pattern UMElem i j k l <- (umElemAsTuple -> (i, j, k, l))
  where
    UMElem i j k l = case (i, j, k, l) of
      (SNothing, SI.Tip, SNothing, SNothing) -> TEEEE
      (SNothing, SI.Tip, SNothing, SJust d) -> TEEEF d
      (SNothing, SI.Tip, SJust s, SNothing) -> TEEFE s
      (SNothing, SI.Tip, SJust s, SJust d) -> TEEFF s d
      (SNothing, p, SNothing, SNothing) -> TEFEE p
      (SNothing, p, SNothing, SJust d) -> TEFEF p d
      (SNothing, p, SJust s, SNothing) -> TEFFE p s
      (SNothing, p, SJust s, SJust d) -> TEFFF p s d
      (SJust r, SI.Tip, SNothing, SNothing) -> TFEEE r
      (SJust r, SI.Tip, SNothing, SJust d) -> TFEEF r d
      (SJust r, SI.Tip, SJust s, SNothing) -> TFEFE r s
      (SJust r, SI.Tip, SJust s, SJust d) -> TFEFF r s d
      (SJust r, p, SNothing, SNothing) -> TFFEE r p
      (SJust r, p, SNothing, SJust d) -> TFFEF r p d
      (SJust r, p, SJust s, SNothing) -> TFFFE r p s
      (SJust r, p, SJust s, SJust d) -> TFFFF r p s d

{-# COMPLETE UMElem #-}

-- | A unified map represents 4 Maps with domain @(Credential 'Staking c)@
--
-- 1) Map (Credential 'Staking c) RDPair  -- (RDPair rewardCoin depositCoin)
-- 2) Map (Credential 'Staking c) (Set Ptr)
-- 3) Map (Credential 'Staking c) (StrictMaybe (KeyHash 'StakePool c))
-- 4) Map (Credential 'Staking c) (StrictMaybe (DRep c))
-- and one more map in the inverse direction with @Ptr@ for keys and @(Credential 'Staking c)@ for values.
data UMap c = UMap
  { umElems :: !(Map (Credential 'Staking c) (UMElem c))
  , umPtrs :: !(Map Ptr (Credential 'Staking c))
  }
  deriving (Show, Eq, Generic, NoThunks, NFData)

umElemsL :: Lens' (UMap c) (Map (Credential 'Staking c) (UMElem c))
umElemsL = lens umElems (\x y -> x {umElems = y})

-- | All maps unrolled. It is important to note that all fields are lazy, because
-- conversion from UMap can be expensive, thus only fields that are forced will incur that
-- conversion overhead.
data StakeCredentials c = StakeCredentials
  { scRewards :: Map (Credential 'Staking c) Coin
  , scDeposits :: Map (Credential 'Staking c) Coin
  , scSPools :: Map (Credential 'Staking c) (KeyHash 'StakePool c)
  , scDReps :: Map (Credential 'Staking c) (DRep c)
  , scPtrs :: Map Ptr (Credential 'Staking c)
  , scPtrsInverse :: Map (Credential 'Staking c) (Set Ptr)
  -- ^ There will be no empty sets in the range
  }
  deriving (Show, Eq, Generic, NoThunks, NFData)

instance Crypto c => ToJSON (UMap c) where
  toJSON = object . toUMapPair
  toEncoding = Aeson.pairs . mconcat . toUMapPair

toUMapPair :: (Aeson.KeyValue e a, Crypto c) => UMap c -> [a]
toUMapPair (UMap !m1 !m2) =
  [ "credentials" .= m1
  , "pointers" .= m2
  ]

instance Crypto c => EncCBOR (UMap c) where
  encCBOR UMap {umElems, umPtrs} =
    encodeListLen 2 <> encodeMap encCBOR encCBOR umElems <> encodeMap encCBOR encCBOR umPtrs

instance Crypto c => DecShareCBOR (UMap c) where
  type Share (UMap c) = (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  decSharePlusCBOR =
    StateT
      ( \(a, b) ->
          decodeRecordNamed "UMap" (const 2) $ do
            umElems <- decodeMap (interns a <$> decCBOR) (decShareCBOR b)
            let a' = internsFromMap umElems <> a
            umPtrs <-
              ifDecoderVersionAtLeast
                (natVersion @9)
                (mempty <$ dropCBOR (Proxy @(Map (Credential 'Staking c) (Set Ptr))))
                $ decodeMap decCBOR (interns a' <$> decCBOR)
            pure (UMap {umElems, umPtrs}, (a', b))
      )

-- | It is worthwhile stating the invariant that holds on a Unified Map.
-- The `umPtrs` and the `ptrT` field of the `umElems` are inverses.
umInvariant :: Credential 'Staking c -> Ptr -> UMap c -> Bool
umInvariant cred ptr UMap {umElems, umPtrs} = forwards && backwards
  where
    forwards =
      case Map.lookup cred umElems of
        Nothing -> cred `notElem` umPtrs
        Just (UMElem _r ptrSet _s _) ->
          not (Set.member ptr ptrSet)
            || ( case Map.lookup ptr umPtrs of
                  Nothing -> False
                  Just cred2 -> cred == cred2
               )
    backwards =
      case Map.lookup ptr umPtrs of
        Nothing -> all (\(UMElem _ ptrSet _ _) -> Set.notMember ptr ptrSet) umElems
        Just cred' ->
          case Map.lookup cred' umElems of
            Nothing -> False
            Just (UMElem _ ptrSet _ _) -> Set.member ptr ptrSet

-- | A `UView` lets one view a `UMap` in @n@ different ways,
-- one for each of the elements in a Unified Element `UMElem` @(4)@
-- A @(UView c key value)@ can be used like a @(Map key value)@.
-- It acts like a map, supporting efficient insert, delete, and lookup operations.
data UView c k v where
  RewDepUView ::
    !(UMap c) ->
    UView c (Credential 'Staking c) RDPair
  PtrUView ::
    !(UMap c) ->
    UView c Ptr (Credential 'Staking c)
  SPoolUView ::
    !(UMap c) ->
    UView c (Credential 'Staking c) (KeyHash 'StakePool c)
  DRepUView ::
    !(UMap c) ->
    UView c (Credential 'Staking c) (DRep c)

-- | Construct a `RewDepUView` from the two maps that make up a `UMap`
rewDepUView ::
  Map (Credential 'Staking c) (UMElem c) ->
  Map Ptr (Credential 'Staking c) ->
  UView c (Credential 'Staking c) RDPair
rewDepUView a b = RewDepUView (UMap a b)

-- | Construct a `PtrUView` from the two maps that make up a `UMap`
ptrUView ::
  Map (Credential 'Staking c) (UMElem c) ->
  Map Ptr (Credential 'Staking c) ->
  UView c Ptr (Credential 'Staking c)
ptrUView a b = PtrUView (UMap a b)

-- | Construct a `SPoolUView` from the two maps that make up a `UMap`
sPoolUView ::
  Map (Credential 'Staking c) (UMElem c) ->
  Map Ptr (Credential 'Staking c) ->
  UView c (Credential 'Staking c) (KeyHash 'StakePool c)
sPoolUView a b = SPoolUView (UMap a b)

-- | Construct a `DRepUView` from the two maps that make up a `UMap`
dRepUView ::
  Map (Credential 'Staking c) (UMElem c) ->
  Map Ptr (Credential 'Staking c) ->
  UView c (Credential 'Staking c) (DRep c)
dRepUView a b = DRepUView (UMap a b)

-- | Extract the underlying `UMap` from a `UView`
unUView :: UView c k v -> UMap c
unUView = \case
  RewDepUView um -> um
  PtrUView um -> um
  SPoolUView um -> um
  DRepUView um -> um

-- | Materialize a real `Map` from a `View`
-- This is expensive, use it wisely (like maybe once per epoch boundary to make a `SnapShot`)
-- See also domRestrictedMap, which domain-restricts before computing a view.
unUnify :: UView c k v -> Map k v
unUnify = \case
  RewDepUView UMap {umElems} -> Map.mapMaybe umElemRDPair umElems
  PtrUView UMap {umPtrs} -> umPtrs
  SPoolUView UMap {umElems} -> Map.mapMaybe umElemSPool umElems
  DRepUView UMap {umElems} -> Map.mapMaybe umElemDRep umElems

-- | Materialize a real `VMap` (Vector Map) from a `UView`
-- This is expensive, use it wisely (like maybe once per epoch boundary to make a `SnapShot`)
unUnifyToVMap :: UView c k v -> VMap.VMap VMap.VB VMap.VB k v
unUnifyToVMap uview = case uview of
  RewDepUView UMap {umElems} ->
    VMap.fromListN (size uview) . Maybe.mapMaybe toRDPair . Map.toList $ umElems
  PtrUView UMap {umPtrs} -> VMap.fromMap umPtrs
  SPoolUView UMap {umElems} ->
    VMap.fromListN (size uview) . Maybe.mapMaybe toSPool . Map.toList $ umElems
  DRepUView UMap {umElems} ->
    VMap.fromListN (size uview) . Maybe.mapMaybe toDRep . Map.toList $ umElems
  where
    toRDPair (key, t) = (,) key <$> umElemRDPair t
    toSPool (key, t) = (,) key <$> umElemSPool t
    toDRep (key, t) = (,) key <$> umElemDRep t

-- | Extract a reward-deposit pairs `Map` from a 'UMap'
rdPairMap :: UMap c -> Map (Credential 'Staking c) RDPair
rdPairMap x = unUnify $ RewDepUView x

-- | Extract a rewards `Map` from a 'UMap'
rewardMap :: UMap c -> Map (Credential 'Staking c) Coin
rewardMap x = Map.map (fromCompact . rdReward) $ unUnify $ RewDepUView x

-- | Extract a compact rewards `Map` from a 'UMap'
compactRewardMap :: UMap c -> Map (Credential 'Staking c) (CompactForm Coin)
compactRewardMap x = Map.map rdReward $ unUnify $ RewDepUView x

-- | Extract a deposits `Map` from a 'UMap'
depositMap :: UMap c -> Map (Credential 'Staking c) Coin
depositMap x = Map.map (fromCompact . rdDeposit) $ unUnify $ RewDepUView x

-- | Extract a pointers `Map` from a 'UMap'
ptrMap :: UMap c -> Map Ptr (Credential 'Staking c)
ptrMap x = unUnify $ PtrUView x

-- | Extract a pointers `Map` from a 'UMap'
invPtrMap :: UMap c -> Map (Credential 'Staking c) (Set Ptr)
invPtrMap UMap {umElems} =
  Map.foldlWithKey'
    (\ans k (UMElem _ ptrSet _ _) -> if Set.null ptrSet then ans else Map.insert k ptrSet ans)
    Map.empty
    umElems

-- | Extract a stake pool delegations `Map` from a 'UMap'
sPoolMap :: UMap c -> Map (Credential 'Staking c) (KeyHash 'StakePool c)
sPoolMap x = unUnify $ SPoolUView x

-- | Extract a delegated-representatives `Map` from a 'UMap'
dRepMap :: UMap c -> Map (Credential 'Staking c) (DRep c)
dRepMap x = unUnify $ DRepUView x

-- | Extract a domain-restricted `Map` of a `UMap`.
-- If `Set k` is small this should be efficient.
domRestrictedMap :: Set k -> UView c k v -> Map k v
domRestrictedMap setk = \case
  RewDepUView UMap {umElems} -> Map.mapMaybe umElemRDPair (Map.restrictKeys umElems setk)
  PtrUView UMap {umPtrs} -> Map.restrictKeys umPtrs setk
  SPoolUView UMap {umElems} -> Map.mapMaybe umElemSPool (Map.restrictKeys umElems setk)
  DRepUView UMap {umElems} -> Map.mapMaybe umElemDRep (Map.restrictKeys umElems setk)

toStakeCredentials :: UMap c -> StakeCredentials c
toStakeCredentials umap =
  StakeCredentials
    { scRewards = rewardMap umap
    , scDeposits = depositMap umap
    , scSPools = sPoolMap umap
    , scDReps = dRepMap umap
    , scPtrs = ptrMap umap
    , scPtrsInverse = invPtrMap umap
    }

domRestrictedStakeCredentials :: Set (Credential 'Staking c) -> UMap c -> StakeCredentials c
domRestrictedStakeCredentials setk UMap {umElems, umPtrs} =
  let umElems' = Map.restrictKeys umElems setk
      ptrs = Map.mapMaybe umElemPtrs umElems'
   in StakeCredentials
        { scRewards = Map.mapMaybe (\e -> fromCompact . rdReward <$> umElemRDPair e) umElems'
        , scDeposits = Map.mapMaybe (\e -> fromCompact . rdDeposit <$> umElemRDPair e) umElems'
        , scSPools = Map.mapMaybe umElemSPool umElems'
        , scDReps = Map.mapMaybe umElemDRep umElems'
        , scPtrs = umPtrs `Map.restrictKeys` fold ptrs
        , scPtrsInverse = ptrs
        }

-- | All `View`s are `Foldable`
instance Foldable (UView c k) where
  foldMap f = \case
    RewDepUView UMap {umElems} -> Map.foldlWithKey accum mempty umElems
      where
        accum ans _ (UMElem (SJust rd) _ _ _) = ans <> f rd
        accum ans _ _ = ans
    -- umInvariant` for `PtrUView` does not matter here. We just return a `Map` and not a `UMap`.
    PtrUView UMap {umPtrs} -> foldMap f umPtrs
    SPoolUView UMap {umElems} -> Map.foldlWithKey accum mempty umElems
      where
        accum ans _ (UMElem _ _ (SJust sd) _) = ans <> f sd
        accum ans _ _ = ans
    DRepUView UMap {umElems} -> Map.foldlWithKey accum mempty umElems
      where
        accum ans _ (UMElem _ _ _ (SJust vd)) = ans <> f vd
        accum ans _ _ = ans

  foldr accum ans0 = \case
    RewDepUView UMap {umElems} -> Map.foldr accum' ans0 umElems
      where
        accum' (UMElem (SJust rd) _ _ _) ans = accum rd ans
        accum' _ ans = ans
    -- umInvariant` for `PtrUView` does not matter here. We just return a `Map` and not a `UMap`.
    PtrUView UMap {umPtrs} -> Map.foldr accum ans0 umPtrs
    SPoolUView UMap {umElems} -> Map.foldr accum' ans0 umElems
      where
        accum' (UMElem _ _ (SJust sd) _) ans = accum sd ans
        accum' _ ans = ans
    DRepUView UMap {umElems} -> Map.foldr accum' ans0 umElems
      where
        accum' (UMElem _ _ _ (SJust vd)) ans = accum vd ans
        accum' _ ans = ans

  foldl' accum ans0 = \case
    RewDepUView UMap {umElems} -> Map.foldl' accum' ans0 umElems
      where
        accum' ans = maybe ans (accum ans) . umElemRDPair
    -- umInvariant` for `PtrUView` does not matter here. We just return a `Map` and not a `UMap`.
    PtrUView UMap {umPtrs} -> Map.foldl' accum ans0 umPtrs
    SPoolUView UMap {umElems} -> Map.foldl' accum' ans0 umElems
      where
        accum' ans = maybe ans (accum ans) . umElemSPool
    DRepUView UMap {umElems} -> Map.foldl' accum' ans0 umElems
      where
        accum' ans = maybe ans (accum ans) . umElemDRep

  length = size

-- | `null` for an `UMElem`
nullUMElem :: UMElem c -> Bool
nullUMElem = \case
  UMElem SNothing ptrSet SNothing SNothing | Set.null ptrSet -> True
  _ -> False

-- | `null` `Maybe` for an `UMElem`
nullUMElemMaybe :: UMElem c -> Maybe (UMElem c)
nullUMElemMaybe = \case
  e | nullUMElem e -> Nothing
  e -> Just e

-- | Construct an empty `UMap`
empty :: UMap c
empty = UMap Map.empty Map.empty

-- | Delete a key and its value from the map-like `UView`, returning a version of the same `UView`.
--
-- In the case of a `PtrUView` we maintain the `umInvariant` and delete the pairs from both
-- `umElems` as well as `umPtrs` of the `UMap`.
delete' :: k -> UView c k v -> UView c k v
delete' key = \case
  RewDepUView UMap {umElems, umPtrs} -> rewDepUView (Map.update go key umElems) umPtrs
    where
      go (UMElem _ ptrSet sPool dRep) = nullUMElemMaybe $ UMElem SNothing ptrSet sPool dRep
  PtrUView UMap {umElems, umPtrs} -> case Map.lookup key umPtrs of
    Nothing -> PtrUView $ UMap umElems umPtrs
    Just cred -> ptrUView (Map.update go cred umElems) (Map.delete key umPtrs)
      where
        go (UMElem rd ptrSet sPool dRep) = nullUMElemMaybe $ UMElem rd (Set.delete key ptrSet) sPool dRep
  SPoolUView UMap {umElems, umPtrs} -> sPoolUView (Map.update go key umElems) umPtrs
    where
      go (UMElem rd ptrSet _ dRep) = nullUMElemMaybe $ UMElem rd ptrSet SNothing dRep
  DRepUView UMap {umElems, umPtrs} -> dRepUView (Map.update go key umElems) umPtrs
    where
      go (UMElem rd ptrSet sPool _) = nullUMElemMaybe $ UMElem rd ptrSet sPool SNothing

delete :: k -> UView c k v -> UMap c
delete k m = unUView $ delete' k m

-- | Insert with combination
--
-- If `k` exists as a key in the (map-like) `UView`:
--
--   1. to keep the old value
--   > insertWith' (\ old new -> old) k v view
--
--   2. to replace the old value with the new value
--   > insertWith' (\ old new -> new) k v view
--
--   3. to combine the old and new values with summation
--   > insertWith' (\ old new -> old + new) k v view
--
-- If `k` does not exist as a key in the `UView`,
--   the combining function is ignored, and
--   the key `k` and the value `v` are inserted into the map-like `UView`
--   > insertWith' ignoredCombiningFunction k v view
insertWith' :: (v -> v -> v) -> k -> v -> UView c k v -> UView c k v
insertWith' combine key val = \case
  RewDepUView UMap {umElems, umPtrs} -> rewDepUView (Map.alter go key umElems) umPtrs
    where
      -- Here 'val' is (CompactForm Coin), but the UMap stores Word64,
      -- so there is some implict coercion going on here using the UMElem pattern
      go = \case
        Nothing -> Just $ UMElem (SJust val) Set.empty SNothing SNothing
        Just (UMElem SNothing ptrSet sPool dRep) -> nullUMElemMaybe $ UMElem (SJust val) ptrSet sPool dRep
        Just (UMElem (SJust old) ptrSet sPool dRep) -> nullUMElemMaybe $ UMElem (SJust $ combine old val) ptrSet sPool dRep
  -- Here `key` is a pointer, and `val` a stake credential.
  -- We use the combining function to combine only the val, i.e. the stake credential.
  -- We do not use the combining function to combine the key, i.e. the pointer.
  -- We also maintain the `umInvariant`.
  PtrUView UMap {umElems, umPtrs} ->
    let
      -- We consider what both the old and new stake credentials are
      -- in order to be able to `retract` the pointer to the old credential later on.
      (oldCred, newCred) = case Map.lookup key umPtrs of
        Nothing -> (val, val)
        Just oldVal -> (oldVal, combine oldVal val)
      -- Delete the old pointer from the set in UMElem, but also delete the whole n-tuple if it goes null.
      -- and, add the new pointer to the set in UMElem after retracting the old one.
      newUmElem = Map.update addPtr newCred $ Map.update delPtr oldCred umElems
        where
          addPtr (UMElem rd ptrSet sPool dRep) = Just $ UMElem rd (Set.insert key ptrSet) sPool dRep
          delPtr (UMElem rd ptrSet sPool dRep) = nullUMElemMaybe $ UMElem rd (Set.delete key ptrSet) sPool dRep
      -- `newUmPtr` replaces the old one if one it exists.
      newUmPtr = Map.insert key newCred umPtrs
     in
      PtrUView $ UMap newUmElem newUmPtr
  SPoolUView UMap {umElems, umPtrs} -> sPoolUView (Map.alter go key umElems) umPtrs
    where
      go = \case
        Nothing -> Just $ UMElem SNothing Set.empty (SJust val) SNothing
        Just (UMElem rd ptrSet SNothing dRep) -> Just $ UMElem rd ptrSet (SJust val) dRep
        Just (UMElem rd ptrSet (SJust old) dRep) -> Just $ UMElem rd ptrSet (SJust $ combine old val) dRep
  DRepUView UMap {umElems, umPtrs} -> dRepUView (Map.alter go key umElems) umPtrs
    where
      go = \case
        Nothing -> Just $ UMElem SNothing Set.empty SNothing $ SJust val
        Just (UMElem rd ptrSet sPool SNothing) -> Just $ UMElem rd ptrSet sPool $ SJust val
        Just (UMElem rd ptrSet sPool (SJust old)) -> Just $ UMElem rd ptrSet sPool $ SJust $ combine old val

insertWith :: (v -> v -> v) -> k -> v -> UView c k v -> UMap c
insertWith combine k v uview = unUView $ insertWith' combine k v uview

insert' :: k -> v -> UView c k v -> UView c k v
insert' = insertWith' $ \_old new -> new

insert :: k -> v -> UView c k v -> UMap c
insert k v uview = unUView $ insert' k v uview

-- | Adjust a `UView`, just like `Map.adjust`.
-- This is implemented only for reward-deposit pairs.
adjust :: (RDPair -> RDPair) -> k -> UView c k RDPair -> UMap c
adjust f k (RewDepUView UMap {umElems, umPtrs}) = UMap (Map.adjust go k umElems) umPtrs
  where
    go (UMElem (SJust rd) ptrSet sPool dRep) = UMElem (SJust (f rd)) ptrSet sPool dRep
    go (UMElem SNothing ptrSet sPool dRep) = UMElem SNothing ptrSet sPool dRep

-- | Lookup a `UView`, just like `Map.lookup`.
lookup :: k -> UView c k v -> Maybe v
lookup key = \case
  RewDepUView UMap {umElems} -> Map.lookup key umElems >>= umElemRDPair
  PtrUView UMap {umPtrs} -> Map.lookup key umPtrs
  SPoolUView UMap {umElems} -> Map.lookup key umElems >>= umElemSPool
  DRepUView UMap {umElems} -> Map.lookup key umElems >>= umElemDRep

-- | `null` for a `UView`, just like `Map.null`
nullUView :: UView c k v -> Bool
nullUView = \case
  RewDepUView UMap {umElems} -> all (isNothing . umElemRDPair) umElems
  PtrUView UMap {umPtrs} -> Map.null umPtrs
  SPoolUView UMap {umElems} -> all (isNothing . umElemSPool) umElems
  DRepUView UMap {umElems} -> all (isNothing . umElemDRep) umElems

-- | Get the domain of the `Map`-like `UView`
domain :: UView c k v -> Set k
domain = \case
  RewDepUView UMap {umElems} -> Map.foldlWithKey' accum Set.empty umElems
    where
      accum ans k (UMElem (SJust _) _ _ _) = Set.insert k ans
      accum ans _ _ = ans
  PtrUView UMap {umPtrs} -> Map.keysSet umPtrs
  SPoolUView UMap {umElems} -> Map.foldlWithKey' accum Set.empty umElems
    where
      accum ans k (UMElem _ _ (SJust _) _) = Set.insert k ans
      accum ans _ _ = ans
  DRepUView UMap {umElems} -> Map.foldlWithKey' accum Set.empty umElems
    where
      accum ans k (UMElem _ _ _ (SJust _)) = Set.insert k ans
      accum ans _ _ = ans

-- | Get the range of the `Map`-like `UView`
range :: UView c k v -> Set v
range = \case
  RewDepUView UMap {umElems} -> Map.foldl' accum Set.empty umElems
    where
      accum ans (UMElem (SJust rd) _ _ _) = Set.insert rd ans
      accum ans _ = ans
  PtrUView UMap {umPtrs} -> Set.fromList $ Map.elems umPtrs
  SPoolUView UMap {umElems} -> Map.foldl' accum Set.empty umElems
    where
      accum ans (UMElem _ _ (SJust sPool) _) = Set.insert sPool ans
      accum ans _ = ans
  DRepUView UMap {umElems} -> Map.foldl' accum Set.empty umElems
    where
      accum ans (UMElem _ _ _ (SJust dRep)) = Set.insert dRep ans
      accum ans _ = ans

-- | Union with left preference.
-- So if k, already exists, do nothing, if it doesn't exist insert it.
--
-- Spec:
-- evalUnified (RewDepUView u1 ∪ singleton hk mempty)
-- evalUnified (Ptrs u2 ∪ singleton ptr hk)
unionL, (∪) :: UView c k v -> (k, v) -> UMap c
view ∪ (k, v) = insertWith const k v view
unionL = (∪)

-- | Union with right preference.
-- So if k, already exists, then old v is overwritten with the new v.
--
-- Special rules apply for the `RewDepUView`, where only the `rdReward`
-- field of the `RDPair` is overwritten, and the old `rdDeposit` value persists.
--
-- Note: In this case it is an invariant that the domain of the `Map` on the right side
-- is a subset of the domain of the RewDepUView. See the single case in
-- module Cardano.Ledger.Shelley.Rules.Delegs, in the dealing with Withdrawals's where
-- it is used at this type.
--
-- Spec:
-- evalUnified (delegations ds ⨃ singleton hk dpool)
-- evalUnified (rewards' ⨃ wdrls_')
unionR, (⨃) :: UView c k v -> Map k v -> UMap c
(RewDepUView UMap {umElems, umPtrs}) ⨃ rightUmap = UMap (Map.foldlWithKey' accum umElems rightUmap) umPtrs
  where
    accum !ans k (RDPair r _) = Map.adjust overwrite k ans
      where
        overwrite (UMElem (SJust (RDPair _ d)) ptrSet sPool dRep) = UMElem (SJust (RDPair r d)) ptrSet sPool dRep
        overwrite x = x
view ⨃ mp = unUView $ Map.foldlWithKey' accum view mp
  where
    accum ans k v = insertWith' (\_old new -> new) k v ans
unionR = (⨃)

-- | Add the reward from the `Map` on the right side to the reward in the `UView` on the left.
-- This is only implemented and is applicable to `RewDepUView`s.
--
-- We presume that the domain of the `Map` on the right, is a subset of the domain of the `UView` on the left.
--
-- Spec:
-- evalUnified (rewards dState ∪+ registeredAggregated)
-- evalUnified (rewards' ∪+ update)
-- evalUnified (RewDepUView u0 ∪+ refunds)
unionRewAgg
  , (∪+) ::
    UView c (Credential 'Staking c) RDPair ->
    Map (Credential 'Staking c) (CompactForm Coin) ->
    UMap c
unionRewAgg view m = Map.foldlWithKey' accum (unUView view) m
  where
    accum umap key ccoin = adjust combine key (RewDepUView umap)
      where
        combine (RDPair r d) = RDPair (addCompact r ccoin) d
(∪+) = unionRewAgg

-- | Add the deposit from the `Map` on the right side to the deposit in the `UView` on the left.
-- This is only implemented and is applicable to `RewDepUView`s.
unionKeyDeposits :: UView c k RDPair -> Map k (CompactForm Coin) -> UMap c
unionKeyDeposits view m = unUView $ Map.foldlWithKey' accum view m
  where
    accum vw key ccoin = insertWith' combine key (RDPair (CompactCoin 0) ccoin) vw
    -- If the key isn't present in the `UMap` the combining function is ignored
    -- and the new `RDPair` is inserted in the `UMap`. Ref: haddock for `insertWith'`.
    combine (RDPair r d) (RDPair _ newD) = RDPair r (addCompact d newD)

-- | Delete all keys in the given `Set` from the domain of the given map-like `UView`.
--
-- Spec:
-- evalUnified (setSingleton hk ⋪ RewDepUView u0)
-- evalUnified (setSingleton hk ⋪ SPoolUView u1)
domDelete, (⋪) :: Set k -> UView c k v -> UMap c
set ⋪ view = unUView (Set.foldl' (flip delete') view set)
domDelete = (⋪)

-- | Delete the stake credentials in the domain and all associated ranges from the `UMap`
-- This can be expensive when there are many pointers associated with the credential.
domDeleteAll :: Set (Credential 'Staking c) -> UMap c -> UMap c
domDeleteAll ks umap = Set.foldr' deleteStakingCredential umap ks

-- | Completely remove the staking credential from the UMap, including all associated
-- pointers.
deleteStakingCredential :: Credential 'Staking c -> UMap c -> UMap c
deleteStakingCredential cred = snd . extractStakingCredential cred

-- | Just like `deleteStakingCredential`, but also returned the removed element.
extractStakingCredential :: Credential 'Staking c -> UMap c -> (Maybe (UMElem c), UMap c)
extractStakingCredential cred umap@UMap {umElems, umPtrs} =
  case MapExtras.extract cred umElems of
    (Nothing, _) -> (Nothing, umap)
    (e@(Just (UMElem _ ptrs _ _)), umElems') ->
      ( e
      , UMap
          { umElems = umElems'
          , umPtrs = umPtrs `Map.withoutKeys` ptrs
          }
      )

-- | Delete all elements in the given `Set` from the range of the given map-like `UView`.
-- This is slow for SPoolUView, RewDepUView, and DReps UViews, better hope the sets are small
--
-- Spec:
-- evalUnified (Ptrs u2 ⋫ setSingleton hk)
-- evalUnified (SPoolUView u1 ⋫ retired)
rngDelete, (⋫) :: UView c k v -> Set v -> UMap c
RewDepUView UMap {umElems, umPtrs} ⋫ rdSet = UMap (Map.foldlWithKey' accum umElems umElems) umPtrs
  where
    accum ans key = \case
      UMElem (SJust rd) _ _ _
        | Set.member rd rdSet ->
            let go (UMElem _ ptrSet sPool dRep) = nullUMElemMaybe $ UMElem SNothing ptrSet sPool dRep
             in Map.update go key ans
      _ -> ans
PtrUView um ⋫ set = Set.foldl' rmCred um set
  where
    rmCred m@UMap {umElems, umPtrs} cred = case Map.lookup cred umElems of
      Nothing -> m
      Just (UMElem _ kset _ _) ->
        let go (UMElem rd _ sPool dRep) = nullUMElemMaybe $ UMElem rd Set.empty sPool dRep
         in UMap (Map.update go cred umElems) (foldr Map.delete umPtrs kset)
SPoolUView UMap {umElems, umPtrs} ⋫ sPoolSet = UMap (Map.foldlWithKey' accum umElems umElems) umPtrs
  where
    accum ans key = \case
      UMElem _ _ (SJust sPool) _
        | Set.member sPool sPoolSet ->
            let go (UMElem rd ptrSet _ dRep) = nullUMElemMaybe $ UMElem rd ptrSet SNothing dRep
             in Map.update go key ans
      _ -> ans
DRepUView UMap {umElems, umPtrs} ⋫ dRepSet = UMap (Map.foldlWithKey' accum umElems umElems) umPtrs
  where
    accum ans key = \case
      UMElem _ _ _ (SJust dRep)
        | Set.member dRep dRepSet ->
            let go (UMElem rd ptrSet sPool _) = nullUMElemMaybe $ UMElem rd ptrSet sPool SNothing
             in Map.update go key ans
      _ -> ans
rngDelete = (⋫)

-- | Checks for membership directly against `umElems` instead of a `UView`.
member' :: Credential 'Staking c -> UMap c -> Bool
member' k = Map.member k . umElems

-- | Membership check for a `UView`, just like `Map.member`
--
-- Spec:
-- eval (k ∈ dom (rewards dState))
-- eval (k ∈ dom (rewards ds)))
-- eval (hk ∈ dom (rewards ds))
-- eval (hk ∉ dom (rewards ds))
member, notMember :: k -> UView c k v -> Bool
member k = \case
  RewDepUView UMap {umElems} -> case Map.lookup k umElems of
    Just (UMElem (SJust _) _ _ _) -> True
    _ -> False
  PtrUView UMap {umPtrs} -> Map.member k umPtrs
  SPoolUView UMap {umElems} -> case Map.lookup k umElems of
    Just (UMElem _ _ (SJust _) _) -> True
    _ -> False
  DRepUView UMap {umElems} -> case Map.lookup k umElems of
    Just (UMElem _ _ _ (SJust _)) -> True
    _ -> False
notMember k um = not $ member k um

-- | Domain restriction.
--
-- Spec:
-- eval (dom rewards' ◁ iRReserves (_irwd ds) :: RewardAccounts (Crypto era))
-- eval (dom rewards' ◁ iRTreasury (_irwd ds) :: RewardAccounts (Crypto era))
(◁), domRestrict :: UView c k v -> Map k u -> Map k u
RewDepUView UMap {umElems} ◁ m = intersectDomPLeft p m umElems
  where
    p _ (UMElem (SJust _) _ _ _) = True
    p _ _ = False
PtrUView UMap {umPtrs} ◁ m = Map.intersection m umPtrs
SPoolUView UMap {umElems} ◁ m = intersectDomPLeft p m umElems
  where
    p _ (UMElem _ _ (SJust _) _) = True
    p _ _ = False
DRepUView UMap {umElems} ◁ m = intersectDomPLeft p m umElems
  where
    p _ (UMElem _ _ _ (SJust _)) = True
    p _ _ = False
domRestrict = (◁)

-- | Find the value associated with a key from a `UView`, return the default if the key is not there.
findWithDefault :: v -> k -> UView c k v -> v
findWithDefault def k = fromMaybe def . lookup k

-- | A `UView` is a view, so the size of the view is NOT the same as the size of
-- the underlying `UMElem` map.
size :: UView c k v -> Int
size = \case
  PtrUView UMap {umPtrs} -> Map.size umPtrs
  x -> foldl' (\count _v -> count + 1) 0 x

-- | Create a UMap from 4 separate maps. NOTE: For use in tests only.
unify ::
  Map (Credential 'Staking c) RDPair ->
  Map Ptr (Credential 'Staking c) ->
  Map (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Map (Credential 'Staking c) (DRep c) ->
  UMap c
unify rd ptr sPool dRep = um4
  where
    um1 = unUView $ Map.foldlWithKey' (\um k v -> insert' k v um) (RewDepUView empty) rd
    um2 = unUView $ Map.foldlWithKey' (\um k v -> insert' k v um) (SPoolUView um1) sPool
    um3 = unUView $ Map.foldlWithKey' (\um k v -> insert' k v um) (DRepUView um2) dRep
    um4 = unUView $ Map.foldlWithKey' (\um k v -> insert' k v um) (PtrUView um3) ptr

addCompact :: CompactForm Coin -> CompactForm Coin -> CompactForm Coin
addCompact (CompactCoin x) (CompactCoin y) = CompactCoin (x + y)

sumCompactCoin :: Foldable t => t (CompactForm Coin) -> CompactForm Coin
sumCompactCoin = foldl' addCompact (CompactCoin 0)

sumRewardsUView :: UView c k RDPair -> CompactForm Coin
sumRewardsUView = foldl' accum (CompactCoin 0)
  where
    accum ans (RDPair r _) = addCompact ans r

sumDepositUView :: UView c k RDPair -> CompactForm Coin
sumDepositUView = foldl' accum (CompactCoin 0)
  where
    accum ans (RDPair _ d) = addCompact ans d
