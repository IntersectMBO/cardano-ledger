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
{-# LANGUAGE RecordWildCards #-}

{- | A 'UMap' (for Unified map) represents 

1. 3 Maps with the same domain in one direction, as a single `Map` and
2. 2 other maps which are both inverse of each other. as two `Map`s

The advantage of using `UMap` is that `UMap` stores all the information 
compactly, by exploiting the the large amount of sharing in Map 1. 

As for the other two Maps, we don't expect them to have much volume.

-}
module Cardano.Ledger.UMap (
  -- * Constructing 'UMap'
  -- $ UMap
  RDPair (..),
  Elem (ElemP),
  elemReward,
  elemRewardActiveDelegation,
  elemDelegation,
  UMap (..),
  umInvariant,

  -- * UMap View and its components
  -- $ UMapView
  View (..),
  unView,

  viewToVMap,

  rewardView,
  compactRewardView,
  depositView,
  compactDepositView,
  rdView,
  compactRdView,

  vDelegView,
  sDelegView,

  ptrView,

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

  unify,
  unUnify,

  -- * Set and Map operations on Views
  -- $ UMapViewOperations
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
  unionL,
  
  (⨃),
  unionR,
  
  (∪+),
  unionAddRewards,
  
  unionKeyDeposits,

  (⋪),
  domDelete,
  
  (⋫),
  rngDelete,
  
  (◁),
  domRestrict,

  member,
  notMember,

  -- * Derived functions
  -- $ DerivedFunctions
  findWithDefault,
  size,
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

-- $ Umap

{- | A Reward-Deposit Pair
Used to represent the reward and the deposit for a given (Credential 'Staking c)
-}
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

{- | An `Elem` compactly represents a collection of maps with the same domain 
as a single map with its range as the collection.

This space-compacting datatype, and the pattern `ElemP` are equivalent to:
@
data Elem c = Elem
  { coinT :: !(StrictMaybe RDPair),
    vDelegT :: !(StrictMaybe (Credential 'Voting c)),
    poolidT :: !(StrictMaybe (KeyHash 'StakePool c))
  }
 deriving (Show, Eq, Generic, NoThunks, NFData)
@

To name the constructors of `Elem` we use the notation @Txxx@ where
each @x@ is 
either @F@ for full, i.e. the component is present,
or @E@ for empty, i.e. the component is not present.

There are three components
1) the reward-deposit pair as an `RDPair` (CompactForm Coin) (CompactForm Coin) as a pair of Word64s, the first @x@
2) the voting delegatee id (Credential 'Voting c), the second @x@, and
3) the stake pool id (KeyHash 'StakePool c), the third @x@.

So, 
TEEE means none of the components are present,
TFEE means only the reward-deposit pair is present,
TEFE means only the voting delegatee id is  present, and 
TEEF means only the stake pool id is present. etc.

The pattern 'ElemP' will correctly use the optimal constructor.
-}
data Elem c
  = TEEE
  | TEEF !(KeyHash 'StakePool c)
  | TEFE !(Credential 'Voting c)
  | TEFF !(Credential 'Voting c) !(KeyHash 'StakePool c)
  | TFEE {-# UNPACK #-} !RDPair
  | TFEF {-# UNPACK #-} !RDPair !(KeyHash 'StakePool c)
  | TFFE {-# UNPACK #-} !RDPair !(Credential 'Voting c)
  | TFFF {-# UNPACK #-} !RDPair !(Credential 'Voting c) !(KeyHash 'StakePool c)
  deriving (Eq, Ord, Generic, NoThunks, NFData)

instance (Crypto c) => ToJSON (Elem c) where
  toJSON = object . toElemPair
  toEncoding = Aeson.pairs . mconcat . toElemPair

toElemPair :: (Aeson.KeyValue a, Crypto c) => Elem c -> [a]
toElemPair (ElemP !rd !vdeleg !sdeleg) =
  [ "reward" .= fmap rdReward rd
  , "deposit" .= fmap rdDeposit rd
  , "vdeleg" .= vdeleg
  , "sdeleg" .= sdeleg
  ]

{- | A `Tuple` view of the Elem.
We can view all of the constructors as an ElemP.
-}
viewElem :: 
  Elem c -> 
  (StrictMaybe RDPair, StrictMaybe (Credential 'Voting c), StrictMaybe (KeyHash 'StakePool c))
viewElem TEEE         = (SNothing, SNothing, SNothing)
viewElem (TEEF x)     = (SNothing, SNothing, SJust x)
viewElem (TEFE x)     = (SNothing, SJust x , SNothing)
viewElem (TEFF x y)   = (SNothing, SJust x , SJust y)
viewElem (TFEE x)     = (SJust x , SNothing, SNothing)
viewElem (TFEF x y)   = (SJust x , SNothing, SJust y)
viewElem (TFFE x y)   = (SJust x , SJust y , SNothing)
viewElem (TFFF x y z) = (SJust x , SJust y , SJust z)

{- | Extract a delegated Reward-Deposit Pair if it is present. 
We can tell that the pair is present and active when Txxx has 
an F in the 1st position (present) and 3rd position (delegated).

TFFF and TFEF
 ^ ^      ^ ^

This is equivalent to: pattern (ElemP (SJust c) _ (SJust _)) -> Just c
-}
elemRDActive :: Elem c -> Maybe RDPair
elemRDActive = \case
  TFFF rdA _ _ -> Just rdA
  TFEF rdA _ -> Just rdA
  _ -> Nothing

{- | Extract the Reward-Deposit Pair if it is present. 
We can tell that the reward is present when Txxx has an F in the first position 

TFFF TFFE TFEF TFEE
 ^    ^    ^    ^

This is equivalent to the pattern (ElemP (SJust c) _ _) -> Just c
-}
elemRDPair :: Elem c -> Maybe RDPair
elemRDPair = \case
  TFEE rd -> Just rd
  TFEF rd _ -> Just rd
  TFFE rd _ -> Just rd
  TFFF rd _ _ -> Just rd
  _ -> Nothing

{- | Extract the Voting Delegatee ID, if present. 
We can tell that the delegatee is present when Txxx has an F in the second position 

TFFF TEFE TEFF TFFE
  ^    ^    ^    ^

This is equivalent to the pattern (ElemP _ (SJust v) _) -> Just v
-}
elemVDeleg :: Elem c -> Maybe (Credential 'Voting c)
elemVDeleg = \case
  TFFF _ v _ -> Just v
  TFFE _ v -> Just v
  TEFF v _ -> Just v
  TEFE v -> Just v
  _ -> Nothing

{- | Extract the Stake Delegation Pool ID, if present. 
We can tell that the pool id is present when Txxx has an F in the third position 

TFFF TFEF TEFF TEEF
   ^    ^    ^    ^

This is equivalent to the pattern (ElemP _ _ (SJust p)) -> Just p
-}
elemSDeleg :: Elem c -> Maybe (KeyHash 'StakePool c)
elemSDeleg = \case
  TFFF _ _ p -> Just p
  TFEF _ p -> Just p
  TEFF _ p -> Just p
  TEEF p -> Just p
  _ -> Nothing


-- | An `ElemP` can be extracted and injected into the `TEEE` ... `TFFF` constructors.
pattern ElemP :: 
  StrictMaybe RDPair -> 
  StrictMaybe (Credential 'Voting c) -> 
  StrictMaybe (KeyHash 'StakePool c) -> 
  Elem c
pattern ElemP a b c <- (viewElem -> (a, b, c))
  where
    ElemP a b c =
      case (a, b, c) of
        (SNothing, SNothing, SNothing) -> TEEE
        (SNothing, SNothing, SJust x ) -> TEEF x
        (SNothing, SJust x , SNothing) -> TEFE x
        (SNothing, SJust x , SJust y ) -> TEFF x y
        (SJust x , SNothing, SNothing) -> TFEE x
        (SJust x , SNothing, SJust y ) -> TFEF x y
        (SJust x , SJust y , SNothing) -> TFFE x y
        (SJust x , SJust y , SJust z ) -> TFFF x y z
{-# COMPLETE ElemP #-}

instance Show (Elem c) where
  show (ElemP a b c) = 
    unlines
      [ "(ElemP ("
      , show a <> ", "
      , show b <> ", "
      , show c
      , "))"
      ]

{- | A unified map represents a collection of Maps with a common domain @(Credential 'Staking c)@
for keys as a single map, and two more maps that are inverses of each-other with 
@`Ptr`@ for keys and @`(Credential 'Staking c)`@ for values, and its inverse.
-}
data UMap c = UMap 
  { umCredElem :: !(Map (Credential 'Staking c) (Elem c)) 
  , umPtrCred :: !(Map Ptr (Credential 'Staking c))
  , umCredPtr :: !(Map (Credential 'Staking c) Ptr)
  }
  deriving (Eq, Generic, NoThunks, NFData)

instance Show (UMap c) where
  show (UMap{..}) =
    unlines
      [ "(UMap ("
      , show umCredElem
      , show umPtrCred
      , show umCredPtr
      , "))"
      ]

instance Crypto c => ToJSON (UMap c) where
  toJSON = object . toUMapPair
  toEncoding = Aeson.pairs . mconcat . toUMapPair

toUMapPair :: (Aeson.KeyValue a, Crypto c) => UMap c -> [a]
toUMapPair (UMap !m1 !m2 !m3) =
  [ "elems" .= m1
  , "ptrCreds" .= m2
  , "credPtrs" .= m3
  ]

{- | Invariant for a 'UMap'.

Maps 2 and 3 must be inverses of each other. In other words,
all credentials in umPtrCreds should exist in umCredPtrs and
map to the same pointers that map to them in umPtrCreds.

Note:
@
  ((== ptr) <$> Map.lookup cred credPtrMap) 
    == 
      ((== cred) <$> Map.lookup ptr ptrCredMap)
@
  Means, the equation can evaluate to `True` only and only when either 
    both are 'Nothing', which means they were both not found, or 
    when both are 'Just',
      their contents have to also both 
        be either 'True' or 'False'

In other words, this is equivalent to
@ 
  Map.isSubmapOf (Map.singleton cred ptr) credPtrMap 
    && 
      Map.isSubMapOf (Map.singleton ptr cred) ptrCredMap
@

-}
umInvariant :: Credential 'Staking c -> Ptr -> UMap c -> Bool
umInvariant cred ptr (UMap _elemMap ptrCredMap credPtrMap) = 
  ((== ptr) <$> Map.lookup cred credPtrMap) == ((== cred) <$> Map.lookup ptr ptrCredMap)

  -- TODO: @aniket remove once tests pass
  -- 
  -- -- new
  -- forwards && backwards
  -- where
  --   forwards =
  --     case Map.lookup stake credPtrMap of
  --       Nothing -> all (stake /=) ptrCredMap
  --       Just ptr -> case Map.lookup ptr ptrCredMap of
  -- -- old
  -- where
  --   forwards =
  --     case Map.lookup stake tripmap of
  --       Nothing -> all (stake /=) ptrmap
  --       Just (ElemP _c set _d) ->
  --         if Set.member ptr set
  --           then case Map.lookup ptr ptrmap of
  --             Nothing -> False
  --             Just stake2 -> stake == stake2
  --           else True
  --   backwards =
  --     case Map.lookup ptr ptrmap of
  --       Nothing -> all (\(ElemP _ set _) -> Set.notMember ptr set) tripmap
  --       Just cred ->
  --         case Map.lookup cred tripmap of
  --           Nothing -> False
  --           Just (ElemP _ set _) -> Set.member ptr set

-- $ UMapView

{- | A `View` lets one view a `UMap` in four different ways
A @(View c key value)@ can be used like a @(Map key value)@
-}
data View c k v where
  RewDepView ::
    !(UMap c) ->
    View c (Credential 'Staking c) RDPair
  VDelegView ::
    !(UMap c) ->
    View c (Credential 'Staking c) (Credential 'Voting c)
  SDelegView ::
    !(UMap c) ->
    View c (Credential 'Staking c) (KeyHash 'StakePool c)
  PtrView ::
    !(UMap c) ->
    View c Ptr (Credential 'Staking c)

-- | Construct a `RewDepView` from the three maps that make up a `UMap`
rdPairView ::
  Map (Credential 'Staking c) (Elem c) ->
  Map Ptr (Credential 'Staking c) ->
  Map (Credential 'Staking c) Ptr ->
  View c (Credential 'Staking c) RDPair
rdPairView a b c = RewDepView (UMap a b c)

-- | Construct a `VDelegView` from the three maps that make up a `UMap`
vDelegView ::
  Map (Credential 'Staking c) (Elem c) ->
  Map Ptr (Credential 'Staking c) ->
  Map (Credential 'Staking c) Ptr ->
  View c (Credential 'Staking c) (Credential 'Voting c)
vDelegView a b c = VDelegView (UMap a b c)

-- | Construct a `SDelegView` from the three maps that make up a `UMap`
sDelegView ::
  Map (Credential 'Staking c) (Elem c) ->
  Map Ptr (Credential 'Staking c) ->
  Map (Credential 'Staking c) Ptr ->
  View c (Credential 'Staking c) (KeyHash 'StakePool c)
sDelegView a b c = SDelegView (UMap a b c)

-- | Construct a `PtrView` from the three maps that make up a `UMap`
ptrView ::
  Map (Credential 'Staking c) (Elem c) ->
  Map Ptr (Credential 'Staking c) ->
  Map (Credential 'Staking c) Ptr ->
  View c Ptr (Credential 'Staking c)
ptrView a b c = PtrView (UMap a b c)

-- | Extract the underlying `UMap` from a `View`
unView :: View c k v -> UMap c
unView = \case
  RewDepView um -> um
  VDelegView um -> um
  SDelegView um -> um
  PtrView um -> um

{- | Materialize a real `Map` from a `View`
This is expensive, use it wisely (like maybe once per epoch boundary to make a `SnapShot`)
See also domRestrictedView, which domain-restricts before computing a view.
-}
unUnify :: View c k v -> Map k v
unUnify = \case
  RewDepView UMap{umCredElem} -> Map.mapMaybe elemRDPair umCredElem
  VDelegView UMap{umCredElem} -> Map.mapMaybe elemVDeleg umCredElem
  SDelegView UMap{umCredElem} -> Map.mapMaybe elemSDeleg umCredElem
  PtrView UMap{umPtrCred} -> umPtrCred

{- | Materialize a real `VMap` (Vector Map) from a `View`
This is expensive, use it wisely (like maybe once per epoch boundary to make a `SnapShot`)
-}
viewToVMap :: View c k v -> VMap.VMap VMap.VB VMap.VB k v
viewToVMap view =
  case view of
    RewDepView UMap{umCredElem} ->
      VMap.fromListN (size view) . Maybe.mapMaybe toRewDep . Map.toList $ umCredElem
    VDelegView UMap{umCredElem} ->
      VMap.fromListN (size view) . Maybe.mapMaybe toVDeleg . Map.toList $ umCredElem
    SDelegView UMap{umCredElem} ->
      VMap.fromListN (size view) . Maybe.mapMaybe toSDeleg . Map.toList $ umCredElem
    PtrView UMap{umPtrCred} -> 
      VMap.fromMap umPtrCred
  where
    toRewDep (key, t) = (,) key <$> elemRDPair t
    toVDeleg (key, t) = (,) key <$> elemVDeleg t
    toSDeleg (key, t) = (,) key <$> elemSDeleg t

-- | Extract a reward-deposit pairs `Map` from a 'UMap'
rdPairMap :: UMap c -> Map.Map (Credential 'Staking c) RDPair
rdPairMap x = unUnify $ RewDepView x

-- | Extract a rewards `Map` from a 'UMap'
rewardMap :: UMap c -> Map.Map (Credential 'Staking c) Coin
rewardMap x = Map.map (fromCompact . rdReward) $ unUnify $ RewDepView x

-- | Extract a compact rewards `Map` from a 'UMap'
compactRewardMap :: UMap c -> Map.Map (Credential 'Staking c) (CompactForm Coin)
compactRewardMap x = Map.map rdReward $ unUnify $ RewDepView x

-- | Extract a deposits `Map` from a 'UMap'
depositMap :: UMap c -> Map.Map (Credential 'Staking c) Coin
depositMap x = Map.map (fromCompact . rdDeposit) $ unUnify $ RewDepView x

-- | Extract a stake delegations `Map` from a 'UMap'
vDelegMap :: UMap c -> Map.Map (Credential 'Staking c) (Credential 'Voting c)
vDelegMap x = unUnify $ VDelegView x

-- | Extract a stake delegations `Map` from a 'UMap'
sDelegMap :: UMap c -> Map.Map (Credential 'Staking c) (KeyHash 'StakePool c)
sDelegMap x = unUnify $ SDelegView x

-- | Extract a pointers `Map` from a 'UMap'
ptrMap :: UMap c -> Map.Map Ptr (Credential 'Staking c)
ptrMap x = unUnify $ PtrView x

-- | Extract a domain-restricted `Map` of a `UMap`.
-- If `Set k` is small this should be efficient.
domRestrictedMap :: Set k -> View c k v -> Map.Map k v
domRestrictedMap setk = \case
  RewDepView UMap{umCredElem} -> Map.mapMaybe elemRDPair (Map.restrictKeys umCredElem setk)
  VDelegView UMap{umCredElem} -> Map.mapMaybe elemVDeleg (Map.restrictKeys umCredElem setk)
  SDelegView UMap{umCredElem} -> Map.mapMaybe elemSDeleg (Map.restrictKeys umCredElem setk)
  PtrView UMap{umPtrCred} -> Map.restrictKeys umPtrCred setk

-- | All `View`s are `Foldable`
instance Foldable (View c k) where

  foldMap f = \case
    RewDepView UMap{umCredElem} -> Map.foldlWithKey accum mempty umCredElem
      where
        accum ans _ (ElemP (SJust rd) _ _) = ans <> f rd
        accum ans _ _ = ans
    VDelegView UMap{umCredElem} -> Map.foldlWithKey accum mempty umCredElem
      where
        accum ans _ (ElemP _ (SJust vd) _) = ans <> f vd
        accum ans _ _ = ans
    SDelegView UMap{umCredElem} -> Map.foldlWithKey accum mempty umCredElem
      where
        accum ans _ (ElemP _ _ (SJust sd)) = ans <> f sd
        accum ans _ _ = ans
    PtrView UMap{umPtrCred} -> foldMap f umPtrCred
    -- ^ umInvariant` does not matter here. We just return a `Map` and not a `UMap`.
  
  foldr accum ans0 = \case
    RewDepView UMap{umCredElem} -> Map.foldr accum' ans0 umCredElem
      where
        accum' (ElemP (SJust rd) _ _) ans = accum rd ans
        accum' _ ans = ans
    VDelegView UMap{umCredElem} -> Map.foldr accum' ans0 umCredElem
      where
        accum' (ElemP _ (SJust vd) _) ans = accum vd ans
        accum' _ ans = ans
    SDelegView UMap{umCredElem} -> Map.foldr accum' ans0 umCredElem
      where
        accum' (ElemP _ _ (SJust sd)) ans = accum sd ans
        accum' _ ans = ans
    PtrView UMap{umPtrCred} -> Map.foldr accum ans0 umPtrCred
    -- ^ umInvariant` does not matter here. We just return a `Map` and not a `UMap`.

  foldl' accum ans0 = \case
    RewDepView UMap{umCredElem} ->  Map.foldl' accum' ans0 umCredElem
      where
        accum' ans = maybe ans (accum ans) . elemRDPair
    VDelegView UMap{umCredElem} -> Map.foldl' accum' ans0 umCredElem
      where
        accum' ans = maybe ans (accum ans) . elemVDeleg
    SDelegView UMap{umCredElem} -> Map.foldl' accum' ans0 umCredElem
      where
        accum' ans = maybe ans (accum ans) . elemSDeleg
    PtrView UMap{umPtrCred} -> Map.foldl' accum ans0 umPtrCred
    -- ^ umInvariant` does not matter here. We just return a `Map` and not a `UMap`.

  length = size

-- | `null` for an `Elem`
nullElem :: Elem c -> Bool
nullElem = \case
  ElemP SNothing SNothing SNothing -> True
  _ -> False

-- | `null` `Maybe` for an `Elem`
nullElemMaybe :: Elem c -> Maybe (Elem c)
nullElemMaybe = \case
  e | nullElem e -> Nothing
  e -> Just e

-- $ UMapViewOperations

-- | Construct an empty `UMap`
empty :: UMap c
empty = UMap Map.empty Map.empty Map.empty

{- | Delete a key and its value from the map-like `View`, returning a version of the same `View`.
In the case of a `PtrView` we maintain the `umInvariant` and delete the pairs from both
`umPtrCred` as well as `umCredPtr` of the `UMap`.
-}
delete' :: k -> View c k v -> View c k v
delete' key = \case
  RewDepView UMap{..} -> rdPairView (Map.update go key umCredElem) umPtrCred umCredPtr
    where
      go (ElemP _ vdeleg sdeleg) = nullElemMaybe $ ElemP SNothing vdeleg sdeleg
  VDelegView UMap{..} -> vDelegView (Map.update go key umCredElem) umPtrCred umCredPtr
    where
      go (ElemP rd _ sdeleg) = nullElemMaybe $ ElemP rd SNothing sdeleg
  SDelegView UMap{..} -> sDelegView (Map.update go key umCredElem) umPtrCred umCredPtr
    where
      go (ElemP rd vdeleg _) = nullElemMaybe $ ElemP rd vdeleg SNothing
  -- | `key` is a `Ptr` here.
  PtrView UMap{..} -> case Map.lookup key umPtrCred of -- NOTE: Are more checks necessary here?
    Just cred -> ptrView umCredElem (Map.delete key umPtrCred) (Map.delete cred umCredPtr)
    Nothing -> PtrView UMap{..}

delete :: k -> View c k v -> UMap c
delete k m = unView $ delete' k m

{- | Insert with combination

If `k` exists as a key in the (map-like) `View`:

  1. to keep the old value
  > insertWith' (\ old new -> old) k v view

  2. to replace the old value with the new value
  > insertWith' (\ old new -> new) k v view

  3. to combine the old and new values with summation
  > insertWith' (\ old new -> old + new) k v view

If `k` does not exist as a key in the `View`, 
  the combining function is ignored, and
  the key `k` and the value `v` are inserted into the map-like `View`
  > insertWith' combiningFunction k v view
-}
insertWith' :: (v -> v -> v) -> k -> v -> View c k v -> View c k v
insertWith' combine key val = \case
  RewDepView UMap{..} -> rdPairView (Map.alter go key umCredElem) umPtrCred umCredPtr
    where
      -- | Here 'val' is (CompactForm Coin), but the UMap stores Word64,
      -- so there is some implict coercion going on here using the ElemP pattern
      go Nothing = Just $ ElemP (SJust val) SNothing SNothing
      go Just (ElemP SNothing vd sd) = nullElemMaybe $ ElemP (SJust val) vd sd
      go Just (ElemP (SJust old) vd sd) = nullElemMaybe $ ElemP (SJust $ combine old val) vd sd
  VDelegView UMap{..} -> vDelegView (Map.alter go key umCredElem) umPtrCred umCredPtr
    where
      go Nothing = Just $ ElemP SNothing (SJust val) SNothing
      go Just (ElemP rd SNothing sd) = Just $ ElemP rd (SJust val) sd
      go Just (ElemP rd (SJust old) sd) = Just $ ElemP rd (SJust $ combine old val) sd
  SDelegView UMap{..} -> sDelegView (Map.alter go key umCredElem) umPtrCred umCredPtr
    where
      go Nothing = Just $ ElemP SNothing SNothing (SJust val)
      go Just (ElemP rd vd SNothing) = Just $ ElemP rd vd (SJust val)
      go Just (ElemP rd vd (SJust old)) = Just $ ElemP rd vd (SJust $ combine old val)
  -- | Here `key` is a Ptr.
  -- We use the combining function to combine only the val, i.e. the stake credential.
  -- We do not use the combining function to combine the key, i.e. the pointer.
  PtrView UMap{..} -> ptrView umCredElem (Map.insertWith combine key val umPtrCred) newUmCredPtr
    where
      newUmCredPtr = case Map.lookup key umPtrCred of
        Just oldCred -> Map.insert (combine oldCred val) key $ Map.delete oldCred umCredPtr
        Nothing -> Map.insert val key umCredPtr

insertWith :: (v -> v -> v) -> k -> v -> View c k v -> UMap c
insertWith combine k v m = unView $ insertWith' combine k v m

insert' :: k -> v -> View c k v -> View c k v
insert' = insertWith' $ \_old new -> new

insert :: k -> v -> View c k v -> UMap c
insert k v m = unView $ insert' k v m

adjust :: (RDPair -> RDPair) -> k -> View c k RDPair -> UMap c
adjust f k (RewDepView UMap{..}) = UMap (Map.adjust go k umCredElem) umPtrCred umCredPtr
  where
    go (ElemP (SJust rd) vd sd) = ElemP (SJust (f rd)) vd sd
    go (ElemP SNothing vd sd) = ElemP SNothing vd sd

lookup :: k -> View c k v -> Maybe v
lookup key = \case
  RewDepView UMap{umCredElem} -> Map.lookup key umCredElem >>= elemRDPair
  VDelegView UMap{umCredElem} -> Map.lookup key umCredElem >>= elemVDeleg
  SDelegView UMap{umCredElem} -> Map.lookup key umCredElem >>= elemSDeleg
  PtrView    UMap{umPtrCred} -> Map.lookup key umPtrCred

null :: View c k v -> Bool
null = \case
  RewDepView UMap{umCredElem} -> all (isNothing . elemRDPair) umCredElem
  VDelegView UMap{umCredElem} -> all (isNothing . elemVDeleg) umCredElem
  SDelegView UMap{umCredElem} -> all (isNothing . elemSDeleg) umCredElem
  PtrView    UMap{umPtrCred} -> Map.null umPtrCred

domain :: View c k v -> Set k
domain = \case
  RewDepView UMap{umCredElem} -> Map.foldlWithKey' accum Set.empty umCredElem
    where
      accum ans k (ElemP (SJust _) _ _) = Set.insert k ans
      accum ans _ _ = ans
  VDelegView UMap{umCredElem} -> Map.foldlWithKey' accum Set.empty umCredElem
    where
      accum ans k (ElemP _ (SJust _) _) = Set.insert k ans
      accum ans _ _ = ans
  SDelegView UMap{umCredElem} -> Map.foldlWithKey' accum Set.empty umCredElem
    where
      accum ans k (ElemP _ _ (SJust _)) = Set.insert k ans
      accum ans _ _ = ans
  PtrView UMap{umPtrCred} -> Map.keysSet umPtrCred

range :: View c k v -> Set v
range = \case
  RewDepView UMap{umCredElem} -> Map.foldl' accum Set.empty umCredElem
    where
      accum ans (ElemP (SJust rd) _ _) = Set.insert rd ans
      accum ans _ = ans
  VDelegView UMap{umCredElem} -> Map.foldl' accum Set.empty umCredElem
    where
      accum ans (ElemP _ (SJust vd) _) = Set.insert vd ans
      accum ans _ = ans
  SDelegView UMap{umCredElem} -> Map.foldl' accum Set.empty umCredElem
    where
      accum ans (ElemP _ _ (SJust sd)) = Set.insert sd ans
      accum ans _ = ans
  PtrView UMap{umPtrCred} -> Set.fromList $ Map.elems umPtrCreds

-- TODO: @aniketd continue rewrite from here

-- =============================================================
-- evalUnified (RewardDeposits u1 ∪ singleton hk mempty)
-- evalUnified (Ptrs u2 ∪ singleton ptr hk)

-- | Union with left preference, so if k, already exists, do nothing, if it doesn't exist insert it.
(∪) :: View c k v -> (k, v) -> UMap c
view ∪ (k, v) = insertWith (const old) k v view

unionL = (∪)

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
    accum !ansElemmap k (RDPair ccoin _) = Map.adjust overwrite k ansElemmap
      where
        overwrite (ElemP (SJust (RDPair _ deposit)) a b) = ElemP (SJust (RDPair ccoin deposit)) a b
        overwrite x = x
view ⨃ mp = unView $ Map.foldlWithKey' accum view mp
  where
    accum ans k v = insertWith' (\_old new -> new) k v ans

unionR = (⨃)

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
addCoinToJustRewardsPartOfRDPair SNothing _ = SNothing -- FIXME: @aniketd should the rewards not be added to 0 here?
addCoinToJustRewardsPartOfRDPair (SJust (RDPair rew deposit)) delta =
  SJust (RDPair (addCompact rew delta) deposit)

unionHelp ::
  Ord k =>
  Map k (Elem c) ->
  Map k (CompactForm Coin) ->
  Map k (Elem c)
unionHelp tm mm =
  let f _k (ElemP p1 s deposit) delta =
        Just (ElemP (addCoinToJustRewardsPartOfRDPair p1 delta) s deposit)
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

deleteDom = (⋪)

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
        Just (ElemP _ kset _) ->
          UMap (Map.update ok cred m2) (foldr (\k pset -> Map.delete k pset) m1 kset)
          where
            ok (ElemP coin _ poolid) = zeroMaybe (ElemP coin Set.empty poolid)
        Nothing -> m
Delegations (UMap tmap pmap) ⋫ delegset = UMap (Map.foldlWithKey' accum tmap tmap) pmap
  where
    ok (ElemP c set _) = zeroMaybe (ElemP c set SNothing)
    accum ans _key (ElemP _ _ SNothing) = ans
    accum ans key (ElemP _ _ (SJust d)) =
      if Set.member d delegset
        then Map.update ok key ans
        else ans
RewardDeposits (UMap tmap pmap) ⋫ coinset = UMap (Map.foldlWithKey' accum tmap tmap) pmap
  where
    ok (ElemP _ set d) = zeroMaybe (ElemP SNothing set d)
    accum ans key (ElemP (SJust ccoin) _ _) =
      if Set.member ccoin coinset
        then Map.update ok key ans
        else ans
    accum ans _ _ = ans
    
deleteFromRng = (⋫)

-- =============================================

-- eval (k ∈ dom (rewards dState))
-- eval (k ∈ dom (rewards ds)))
-- eval (hk ∈ dom (rewards ds))
-- eval (hk ∉ dom (rewards ds))

member :: k -> View c k v -> Bool
member k (RewardDeposits (UMap tmap _)) =
  case Map.lookup k tmap of
    Just (ElemP (SJust _) _ _) -> True
    _ -> False
member k (Delegations (UMap tmap _)) =
  case Map.lookup k tmap of
    Just (ElemP _ _ (SJust _)) -> True
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
    p _ (ElemP (SJust _) _ _) = True
    p _ _ = False
domRestrict (Delegations (UMap tmap _)) m = intersectDomPLeft p m tmap
  where
    p _ (ElemP _ _ (SJust _)) = True
    p _ _ = False
domRestrict (Ptrs (UMap _ pmap)) m = Map.intersection m pmap

-- ==========================

instance
  (Crypto c) =>
  EncCBOR (Elem c)
  where
  encCBOR (ElemP coin ptr pool) =
    encodeListLen 3 <> encCBOR coin <> encCBOR ptr <> encCBOR pool

instance Crypto c => DecShareCBOR (Elem c) where
  type Share (Elem c) = Interns (KeyHash 'StakePool c)
  decShareCBOR is =
    decodeRecordNamed `ElemP` (const 3) $
      do
        a <- decCBOR
        b <- decCBOR
        c <- decShareMonadCBOR is
        pure (ElemP a b c)

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
          decodeRecordNamed `UMap` (const 2) $ do
            tripmap <- decodeMap (interns a <$> decCBOR) (decShareCBOR b)
            let a' = internsFromMap tripmap <> a
            ptrmap <- decodeMap decCBOR (interns a' <$> decCBOR)
            pure (UMap tripmap ptrmap, (a', b))
      )

-- $ DerivedFunctions

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

instance ToExpr (Elem c)

instance ToExpr (UMap c)
