{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Sharing (
  Interns,
  interns,
  Iso (..),
  isoL,
  toMemptyLens,
  idL,
  credIso,
  credL,
  keyhashIso,
  keyhashL,
  fstL,
  sndL,
  shareMapL,
  pairL,
  DecShareCBOR (..),
  decNoShareCBOR,
  decSharePlusLensCBOR,
  Credential,
  KeyHash,
  KeyRole (..),
  lift,
  PoolParamShare (..),
  VShare,
  PShare (..),
  DShare (..),
  CertShare (..),
  psCRStakingL,
  psVrfL,
  psHKStakingL,
  psKHStakePoolL,
  dsKHStakePoolL,
  dsPShareL,
  dsCRStakingL,
  dsVrfL,
  dsKHGenesisL,
  dsKHGenesisDelegateL,
  csDShareL,
  vsDRepL,
  vsCommitteeL,
  csVShareL,
  decShareLensSet,
  findOrAdd,
  findOrAddLens,
  findOrAddSet,
  decShareMap,
  decSharePlusMap,
  toPShare,
  merge,
  Shareable (..),
  makeShareList,
  makeShareSet,
  makeShareDecoder,
  mapShare,
)
where

import Cardano.Ledger.Binary (
  -- decodeRecordNamedT,
  -- DecCBOR(..),

  DecCBOR (..),
  DecShareCBOR (..),
  Decoder,
  Interns,
  Iso (..),
  decNoShareCBOR,
  decSharePlusLensCBOR,
  decodeMap,
  interns,
  isoL,
  toMemptyLens,
 )
import Cardano.Ledger.Credential (Credential)
import Lens.Micro (Lens', lens, (&), (.~), (^.), _1, _2)

-- import Cardano.Ledger.Crypto(Crypto)

import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..), VerKeyVRF, coerceKeyRole)
import Control.Monad.Trans (lift)

-- import Data.Semigroup (Semigroup(..))
-- import Data.Monoid (Monoid(..))

import Control.Monad.Trans.State.Strict (StateT, get, put)

-- import Data.Map.Strict(Map)

import Cardano.Ledger.Binary (internsFromSet)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

import Data.Foldable (foldl', foldr')
import qualified Data.Set.Internal as Set

import Cardano.Ledger.Core.Era (Era (..))

-- import Cardano.Ledger.Core(Era(..))

{-
import Cardano.Ledger.PoolParams(PoolParams(..))
import qualified Data.Set as Set
import Cardano.Ledger.Address(RewardAccount(..))

import qualified Data.Set as Set
import Cardano.Ledger.Binary (internsFromMap)
import Control.Monad.Trans.State.Strict(state)
 -}

-- =======================================================
-- Lenses and Isomorphisms to make DecShareCBOR instances
-- By supplying the right Interns objects to sub-calls

idL :: Lens' x x
idL = lens getter setter
  where
    getter x = x
    setter _ x = x

credIso :: Iso (Credential kr1 c) (Credential kr2 c)
credIso = Iso coerceKeyRole coerceKeyRole

credL :: Lens' (Interns (Credential kr1 c)) (Interns (Credential kr2 c))
credL = isoL credIso

keyhashIso :: Iso (KeyHash kr1 c) (KeyHash kr2 c)
keyhashIso = Iso coerceKeyRole coerceKeyRole

keyhashL :: Lens' (Interns (KeyHash kr1 c)) (Interns (KeyHash kr2 c))
keyhashL = isoL keyhashIso

-- | Give _1 a less polymorphic type
--   This allows GHC to infer the type of
--   (toMemptyLens fstL id) to be Lens' c (c, b)
--   Supports debugging getting the correct lenses
fstL :: Lens' (a, b) a
fstL = _1

-- | Give _2 a less polymorphic type
--   This allows GHC to infer the type of
--   (toMemptyLens sndL id) to be Lens' c (a, c)
--   Supports debugging getting the correct lenses
sndL :: Lens' (a, b) b
sndL = _2

-- Builds a lens appropriate for calling the decShareCBOR instance of Map
-- It needs a pair (Interns k,Interns v) The (Interns v) are mostly ignored
-- So it is OK for the Lens to alway use mempty for that.
shareMapL :: Lens' x a -> Lens' x (a, Interns y)
shareMapL l = lens getter setter
  where
    getter x = (x ^. l, mempty)
    setter x (a, _) = x & l .~ a

pairL :: Lens' x a -> Lens' x b -> Lens' x (a, b)
pairL xa xb = lens getter setter
  where
    getter x = (x ^. xa, x ^. xb)
    setter x (a, b) = (x & xa .~ a & xb .~ b)

-- ==================================================
-- Explicit Compound Share types.
-- Start from the Leaves and work your way up.
-- PoolParams -> PState -> DState -> VState -> CertState

data PoolParamShare c = PoolParamShare
  { ppsKeyHashStakePool :: Set (KeyHash 'StakePool c)
  , ppsHash :: Set (Hash c (VerKeyVRF c))
  , ppsKeyHashStaking :: Set (KeyHash 'Staking c)
  , ppsCredStaking :: Set (Credential 'Staking c)
  }

instance Semigroup (PoolParamShare c) where
  PoolParamShare a b c d <> PoolParamShare w x y z = PoolParamShare (a <> w) (b <> x) (c <> y) (d <> z)

instance Monoid (PoolParamShare c) where
  mempty = PoolParamShare mempty mempty mempty mempty

-- Add what ever sharing info exists in a PoolParamShare to a PShare
merge :: PoolParamShare c -> PShare c -> PShare c
merge (PoolParamShare w x y z) (PShare a b c d) =
  PShare (a <> internsFromSet w) (b <> internsFromSet x) (c <> internsFromSet y) (d <> internsFromSet z)

class DecShareCBOR t => Shareable t where
  makeShare :: t -> Share t -> (t, Share t)
  makeShareLens :: Lens' x t -> x -> Share t -> (x, Share t)
  makeShareLens l x s0 = (x & l .~ t, s1)
    where
      (t, s1) = makeShare (x ^. l) s0

makeShareDecoder :: (Shareable t, DecCBOR t) => Share t -> Decoder s (t, Share t)
makeShareDecoder sh = do
  t <- decCBOR
  pure (makeShare t sh)

makeShareSet :: (Ord t, Shareable t) => Set t -> Share t -> (Set t, Share t)
makeShareSet xs s = foldl' accum (Set.empty, s) xs
  where
    accum (ts, share0) t = let (t2, share1) = makeShare t share0 in (Set.insert t2 ts, share1)

makeShareList :: Shareable t => [t] -> Share t -> ([t], Share t)
makeShareList xs s = foldr' accum ([], s) xs
  where
    accum t (ts, share0) = let (t2, share1) = makeShare t share0 in (t2 : ts, share1)

-- | Extend the 'Share v', by adding all the info from each 'v' in the Map.
--   This also use the sharing info on the range of the Map.
mapShare :: Shareable v => Map.Map k v -> Share v -> (Share v, Map.Map k v)
mapShare m s = Map.mapAccum foo s m
  where
    foo sh rng = switch (makeShare rng sh)
    switch (x, y) = (y, x)

-- =====================================================

data PShare c = PShare
  { psKHStakePool :: Interns (KeyHash 'StakePool c)
  , psVrf :: Interns (Hash c (VerKeyVRF c))
  , psHKStaking :: Interns (KeyHash 'Staking c)
  , psCRStaking :: Interns (Credential 'Staking c)
  }

instance Semigroup (PShare c) where
  PShare a b c d <> PShare w x y z = PShare (a <> w) (b <> x) (c <> y) (d <> z)

instance Monoid (PShare c) where
  mempty = PShare mempty mempty mempty mempty

psKHStakePoolL :: Lens' (PShare c) (Interns (KeyHash 'StakePool c))
psKHStakePoolL = lens psKHStakePool (\x y -> x {psKHStakePool = y})

psVrfL :: Lens' (PShare c) (Interns (Hash c (VerKeyVRF c)))
psVrfL = lens psVrf (\x y -> x {psVrf = y})

psHKStakingL :: Lens' (PShare c) (Interns (KeyHash 'Staking c))
psHKStakingL = lens psHKStaking (\x y -> x {psHKStaking = y})

psCRStakingL :: Lens' (PShare c) (Interns (Credential 'Staking c))
psCRStakingL = lens psCRStaking (\x y -> x {psCRStaking = y})

-- =======================================================

data DShare era = DShare
  { dsPShare :: PShare (EraCrypto era)
  , dsKHGenesisDelegate :: Interns (KeyHash 'GenesisDelegate (EraCrypto era))
  , dsKHGenesis :: Interns (KeyHash 'Genesis (EraCrypto era))
  }

instance Semigroup (DShare c) where
  DShare a b c <> DShare w x y = DShare (a <> w) (b <> x) (c <> y)

instance Monoid (DShare c) where
  mempty = DShare mempty mempty mempty

dsPShareL :: Lens' (DShare era) (PShare (EraCrypto era))
dsPShareL = lens dsPShare (\x y -> x {dsPShare = y})

dsCRStakingL :: Lens' (DShare era) (Interns (Credential 'Staking (EraCrypto era)))
dsCRStakingL = dsPShareL . psCRStakingL

dsVrfL :: Lens' (DShare era) (Interns (Hash (EraCrypto era) (VerKeyVRF (EraCrypto era))))
dsVrfL = dsPShareL . psVrfL

dsKHStakePoolL :: Lens' (DShare era) (Interns (KeyHash 'StakePool (EraCrypto era)))
dsKHStakePoolL = dsPShareL . psKHStakePoolL

dsKHGenesisDelegateL :: Lens' (DShare era) (Interns (KeyHash 'GenesisDelegate (EraCrypto era)))
dsKHGenesisDelegateL = lens dsKHGenesisDelegate (\x y -> x {dsKHGenesisDelegate = y})

dsKHGenesisL :: Lens' (DShare era) (Interns (KeyHash 'Genesis (EraCrypto era)))
dsKHGenesisL = lens dsKHGenesis (\x y -> x {dsKHGenesis = y})

-- ========================

data VShare era = VShare
  { vsDRep :: Interns (Credential 'DRepRole (EraCrypto era))
  , vsCommittee :: Interns (Credential 'ColdCommitteeRole (EraCrypto era))
  }

instance Semigroup (VShare c) where
  VShare a b <> VShare w x = VShare (a <> w) (b <> x)

instance Monoid (VShare c) where
  mempty = VShare mempty mempty

vsDRepL :: Lens' (VShare era) (Interns (Credential 'DRepRole (EraCrypto era)))
vsDRepL = lens vsDRep (\x y -> x {vsDRep = y})

vsCommitteeL :: Lens' (VShare era) (Interns (Credential 'ColdCommitteeRole (EraCrypto era)))
vsCommitteeL = lens vsCommittee (\x y -> x {vsCommittee = y})

-- ===================================

data CertShare era = CertShare
  { csDShare :: DShare era
  , csVShare :: VShare era
  }

instance Semigroup (CertShare c) where
  CertShare a b <> CertShare w x = CertShare (a <> w) (b <> x)

instance Monoid (CertShare c) where
  mempty = CertShare mempty mempty

csDShareL :: Lens' (CertShare era) (DShare era)
csDShareL = lens csDShare (\x y -> x {csDShare = y})

csVShareL :: Lens' (CertShare era) (VShare era)
csVShareL = lens csVShare (\x y -> x {csVShare = y})

-- ==================================

decShareMap ::
  ( Ord k
  , DecShareCBOR k
  , DecShareCBOR v
  ) =>
  (Share k, Share v) ->
  Decoder s (Map.Map k v)
decShareMap (k, v) =
  decodeMap (decShareCBOR k) (decShareCBOR v)

decSharePlusMap ::
  ( Ord k
  , DecShareCBOR k
  , DecShareCBOR v
  ) =>
  (Map.Map k v -> (Share k, Share v)) ->
  StateT (Share k, Share v) (Decoder s) (Map.Map k v)
decSharePlusMap getShareMap = do
  s <- get
  x <- lift $ decShareMap s
  put (getShareMap x <> s)
  pure x

instance Semigroup (PoolParamX c) where
  PoolParamX a b c d <> PoolParamX w x y z = PoolParamX (a <> w) (b <> x) (c <> y) (d <> z)

instance Monoid (PoolParamX c) where
  mempty = PoolParamX mempty mempty mempty mempty

toPShare :: PoolParamX c -> PShare c
toPShare (PoolParamX a b c d) =
  PShare
    (internsFromSet a)
    (internsFromSet b)
    (internsFromSet c)
    (internsFromSet d)

{-

abstract :: PoolParams c -> PoolParamX c
abstract x = PoolParamX (one(ppId x))
                        (one (ppVrf x))
                        (ppOwners x)
                        (one (raCredential (ppRewardAccount x)))
  where one = Set.singleton

summary :: Ord k => Map k (PoolParams c) -> PShare c
summary m = toPShare $ Map.foldl' accum mempty m
  where accum ans x = ans <> abstract x

getSharePoolMap ::
  Map (KeyHash 'StakePool c) (PoolParams c) ->
  (Interns (KeyHash 'StakePool c), PoolParamShare c )
getSharePoolMap m = (internsFromMap m, summary m)
-}

data PoolParamX c
  = PoolParamX
      (Set (KeyHash 'StakePool c))
      (Set (Hash c (VerKeyVRF c)))
      (Set (KeyHash 'Staking c))
      (Set (Credential 'Staking c))

findOrAdd :: Ord k => k -> Set k -> (k, Set k)
findOrAdd k set = go set
  where
    go Set.Tip = (k, Set.insert k set)
    go (Set.Bin _ kx l r) =
      case compare k kx of
        LT -> go l
        GT -> go r
        EQ -> (kx, set)

findOrAddLens :: Ord k => Lens' t k -> t -> Set k -> (t, Set k)
findOrAddLens l t set = (t & l .~ a, b)
  where
    (a, b) = findOrAdd (t ^. l) set

-- | Carefull, the first arg 'ts0' is being transformed, the share is the second arg 's0'
findOrAddSet :: Ord k => Set k -> Set k -> (Set k, Set k)
findOrAddSet ts0 s0 = Set.foldl' accum (Set.empty, s0) ts0
  where
    accum (ts1, s1) t = let (t', s2) = findOrAdd t s1 in (Set.insert t' ts1, s2)

decShareLensSet ::
  (Ord a, DecCBOR a) =>
  Lens' bs (Set a) ->
  StateT bs (Decoder s) a
decShareLensSet l = do
  bs <- get
  k <- lift decCBOR
  let (x, set) = findOrAdd k (bs ^. l)
  put (bs & l .~ set)
  pure x

{-
shareMap ::
  (Ord k) =>
  StateT bs (Decoder s) k ->
  StateT bs (Decoder s) v ->
  StateT bs (Decoder s) (Map k v)
shareMap kshare vshare = lift (decodeMap <$> kshare <*> vshare)
-}
