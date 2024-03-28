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
  pairL,
  DecShareCBOR (..),
  decNoShareCBOR,
  decSharePlusLensCBOR,
  Credential,
  KeyHash,
  KeyRole (..),
  lift,
  PoolParamShare,
  VShare,
  PShare(..),
  DShare(..),
  CertShare(..),

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
  csCRDRepRoleL,
  csVShareL,

  decShareMap,
  decSharePlusMap,
  toPShare,
 )
where

import Cardano.Ledger.Binary (
  DecShareCBOR (..),
  Interns,
  Iso (..),
  decNoShareCBOR,
  decSharePlusLensCBOR,
  interns,
  isoL,
  toMemptyLens,
  -- decodeRecordNamedT,
  -- DecCBOR(..),
  Decoder,
  decodeMap,
 )
import Cardano.Ledger.Credential (Credential)
import Lens.Micro (Lens', lens, _1, _2, (^.))
-- import Cardano.Ledger.Crypto(Crypto)
import Control.Monad.Trans (lift)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..), Hash, VerKeyVRF,coerceKeyRole)
-- import Data.Semigroup (Semigroup(..))
-- import Data.Monoid (Monoid(..))

import Control.Monad.Trans.State.Strict(StateT,get,put)
import Data.Map.Strict(Map)
import Data.Set(Set)
import Cardano.Ledger.Binary (internsFromSet)


import Cardano.Ledger.Core.Era(Era(..))
-- import Cardano.Ledger.Core(Era(..))
{-
import Cardano.Ledger.PoolParams(PoolParams(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Cardano.Ledger.Address(RewardAccount(..))
import Cardano.Ledger.Binary (internsFromMap)
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

pairL :: Lens' x a -> Lens' x (a,x)
pairL l = lens getter setter
  where getter x = (x ^. l,x)
        setter _ ( _ ,y) = y

-- ==================================================
-- Explicit Compound Share types. 
-- Start from the Leaves and work your way up. 
-- PoolParams -> PState -> DState -> VState -> CertState

type PoolParamShare c = PShare c

data PShare c =
   PShare 
         { psKHStakePool :: Interns (KeyHash 'StakePool c)
         , psVrf :: Interns (Hash c (VerKeyVRF c))
         , psHKStaking :: Interns (KeyHash 'Staking c) 
         , psCRStaking :: Interns (Credential 'Staking c) }

instance Semigroup (PShare c) where
   PShare a b c d <> PShare w x y z = PShare (a<>w) (b<>x) (c<>y) (d<>z)

instance Monoid(PShare c) where
   mempty = PShare mempty mempty mempty mempty

psKHStakePoolL :: Lens' (PoolParamShare c) (Interns (KeyHash 'StakePool c))
psKHStakePoolL = lens psKHStakePool (\ x y -> x{psKHStakePool =y})

psVrfL :: Lens' (PoolParamShare c) (Interns (Hash c (VerKeyVRF c)))
psVrfL = lens psVrf (\ x y -> x{psVrf =y})

psHKStakingL :: Lens' (PoolParamShare c) (Interns (KeyHash 'Staking c))
psHKStakingL = lens psHKStaking (\ x y -> x{psHKStaking = y})

psCRStakingL :: Lens' (PoolParamShare c) (Interns (Credential 'Staking c))
psCRStakingL = lens psCRStaking (\ x y -> x{psCRStaking = y})

-- =======================================================

data DShare era =
   DShare { dsPShare :: PShare (EraCrypto era)
          , dsKHGenesisDelegate :: Interns (KeyHash 'GenesisDelegate (EraCrypto era))
          , dsKHGenesis :: Interns (KeyHash 'Genesis (EraCrypto era)) }

instance Semigroup (DShare c) where
   DShare a b c <> DShare w x y = DShare (a<>w) (b<>x) (c<>y)

instance Monoid(DShare c) where
   mempty = DShare mempty mempty mempty

dsPShareL :: Lens' (DShare era) (PShare (EraCrypto era))
dsPShareL = lens dsPShare (\ x y -> x{dsPShare = y})


dsCRStakingL:: Lens' (DShare era) (Interns (Credential 'Staking (EraCrypto era)))
dsCRStakingL = dsPShareL . psCRStakingL


dsVrfL ::  Lens' (DShare era) (Interns (Hash (EraCrypto era) (VerKeyVRF (EraCrypto era))))
dsVrfL = dsPShareL . psVrfL

dsKHStakePoolL ::  Lens' (DShare  era) (Interns (KeyHash 'StakePool (EraCrypto era)))
dsKHStakePoolL = dsPShareL . psKHStakePoolL


dsKHGenesisDelegateL :: Lens' (DShare era) (Interns (KeyHash 'GenesisDelegate (EraCrypto era)))
dsKHGenesisDelegateL = lens dsKHGenesisDelegate (\ x y -> x{dsKHGenesisDelegate =y})

dsKHGenesisL ::  Lens' (DShare era) (Interns (KeyHash 'Genesis (EraCrypto era)))
dsKHGenesisL = lens dsKHGenesis (\ x y -> x {dsKHGenesis = y})


-- ========================

type VShare era = Interns (Credential 'DRepRole (EraCrypto era))

data CertShare era =
   CertShare { csDShare:: DShare era
          , csCRDRepRole :: (Interns (Credential 'DRepRole (EraCrypto era))) }

instance Semigroup (CertShare c) where
   CertShare a b <> CertShare w x  = CertShare (a<>w) (b<>x) 

instance Monoid(CertShare c) where
   mempty = CertShare mempty mempty  

csDShareL:: Lens' (CertShare era) (DShare era)
csDShareL = lens csDShare (\ x y -> x{csDShare = y})

csCRDRepRoleL :: Lens' (CertShare era) (Interns (Credential 'DRepRole (EraCrypto era)))
csCRDRepRoleL = lens csCRDRepRole (\ x y -> x{csCRDRepRole =y})

csVShareL :: Lens' (CertShare era) (VShare era)
csVShareL = csCRDRepRoleL


-- ==================================

decShareMap :: 
 (Ord k,
  DecShareCBOR k,
  DecShareCBOR v) =>
 (Share k,Share v) -> Decoder s (Map k v)
decShareMap (k,v) = 
  decodeMap (decShareCBOR k) (decShareCBOR v)

decSharePlusMap :: 
 (Ord k,
  DecShareCBOR k,
  DecShareCBOR v) =>
  (Map k v -> (Share k,Share v)) ->
  StateT (Share k, Share v) (Decoder s) (Map k v)
decSharePlusMap getShareMap = do
    s <- get
    x <- lift $ decShareMap s
    put(getShareMap x <> s)
    pure x
 

data PoolParamX c = 
      PoolParamX (Set (KeyHash 'StakePool c))
                 (Set (Hash c (VerKeyVRF c)))
                 (Set (KeyHash 'Staking c))
                 (Set (Credential 'Staking c))                 

instance Semigroup (PoolParamX c) where
   PoolParamX a b c d <> PoolParamX w x y z = PoolParamX (a<>w) (b<>x) (c<>y) (d<>z)

instance Monoid (PoolParamX c) where 
   mempty = PoolParamX mempty mempty mempty mempty

toPShare :: PoolParamX c -> PShare c 
toPShare (PoolParamX a b c d) = 
   PShare (internsFromSet a) 
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