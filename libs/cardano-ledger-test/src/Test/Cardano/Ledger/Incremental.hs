{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module is work in progress, exploring how we might
--   compute parts of the ledger state in an incremental
--   way. Here we focus on stake distribution relations.
--   The idea is that 'Stake' is measured in 'Coin', and a
--   stake distribution relation is a (Map xx Coin), where
--   type 'xx' is some way of distributing some amount of
--   Coin, over a finite domain. Examples of 'xx' include
--   staking credentials, Voting representatives, Ptrs, etc.
module Test.Cardano.Ledger.Incremental where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Coin (Coin (..), Diff (DiffCoin))
import Cardano.Ledger.Core (EraTxOut (..), TxOut, coinTxOutL)
import Cardano.Ledger.Credential (Credential (..), Ptr (..), StakeReference (..))
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Shelley.LedgerState (LedgerState (..)) --  DPState (..), DState (..), PState (..), UTxOState (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UMapCompact (MapLike (..), View (..))
import qualified Cardano.Ledger.UMapCompact as UM
import Cardano.Ledger.UTxO (UTxO (..))
import Control.DeepSeq (NFData (..))
import Data.Incremental (
  BinaryRngD (..),
  Diff (Dm, Dn),
  ILC (..),
  MonoidMap (..),
  MonoidRngD (..),
  insertC,
  inter3C,
  monoidInsertWith,
 )
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import GHC.Generics (Generic (..))
import Lens.Micro
import Test.Cardano.Data (plusBinary, plusUnary)
import Test.Cardano.Ledger.Constrained.Lenses
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (ShelleyEra, Standard)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (Fixed, total)

type TT = ShelleyEra Standard

-- ==================================================================================

-- | Our problem in a nutshell, (Map cred Coin) is a finite source of Coin.
--   (Map cred drep) assigns a 'drep' to each 'cred', Note that more than one 'cred'
--   can assign its Coin to the same 'drep'. The question is, how much Coin is assigned
--   to each 'drep'. We are going to use the incremental lambda calculus to make this
--   kind of computation efficient. So it is cast in terms of things for which we have
--   ILC instances. Note that we wrap (Map cred Coin) and (Map drep Coin) in the MonoidalMap newtype
--   and that we depend upon (and enforce using monoidInsert) that no (Coin 0) entrys are
--   in the input map or are added to the output map.
agg :: (Ord cred, Ord drep) => MonoidMap cred Coin -> Map cred drep -> MonoidMap drep Coin
agg (MM cc) cd = Map.foldlWithKey' accum (MM Map.empty) cc
  where
    accum ans cred coin =
      case Map.lookup cred cd of
        Just drep -> monoidInsertWith drep coin ans
        Nothing -> ans

-- | agg' considers changes in two of its inputs, So we need to write 3 'partial derivatives'
--   1) considers changes only on M
--   2) considers changes on both M and N
--   3) considers changes only on N
--  The function 'interC3' ties them all together
--  Note also that we wrap (Map cred Coin) and (Map drep Coin) in the MonoidalMap
--  newtype to get the right ILC instances. We can easliy unwrap them we we use
--  this function.
agg' ::
  (Show k, Ord k, Show drep, Ord drep) =>
  MonoidMap k Coin ->
  Diff (MonoidMap k Coin) ->
  Map k drep ->
  Diff (Map k drep) ->
  Diff (MonoidMap drep Coin)
agg' (MM m) (Dm dm) n (Dn dn) =
  Dm $
    inter3C Map.empty dm dn (changeDm2 m n) (changeDmDn2 m n) (changeDn2 m n)

traceOn :: Bool
traceOn = False

try :: (Show cred, Show x) => cred -> x -> x
try cred x =
  if traceOn
    then trace ("cred=" ++ show cred ++ " " ++ show x) x
    else x

{-
changeDm ::
  (Show cred, Ord cred, Ord drep, Show drep) =>
  Map cred Coin ->
  Map cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  MonoidRngD (Diff Coin) ->
  Map drep (MonoidRngD (Diff Coin))
changeDm m n ans cred dcoin = case try cred (dcoin, Map.lookup cred m, Map.lookup cred n) of
  (Del, Nothing, Nothing) -> ans
  (Del, Nothing, Just _) -> ans
  (Del, Just _, Nothing) -> ans
  (Del, Just (Coin c2), Just r2) -> insertC r2 (Comb (DiffCoin (-c2))) ans
  (Write _, Nothing, Nothing) -> ans
  (Write c1, Nothing, Just r2) -> insertC r2 (Comb c1) ans
  (Write _, Just _, Nothing) -> ans
  (Write (DiffCoin c1), Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (c1 - c2))) ans
  (Comb _, Nothing, Nothing) -> ans
  (Comb c1, Nothing, Just r2) -> insertC r2 (Comb c1) ans
  (Comb _, Just _, Nothing) -> ans
  (Comb (DiffCoin c1), Just _, Just r2) -> insertC r2 (Comb (DiffCoin c1)) ans

changeDmDn ::
  (Show cred, Ord cred, Show drep, Ord drep) =>
  Map cred Coin ->
  Map cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  (MonoidRngD (Diff Coin), BinaryRngD drep) ->
  Map drep (MonoidRngD (Diff Coin))
changeDmDn m n ans cred (dcoin, drep) = case try cred (dcoin, drep, Map.lookup cred m, Map.lookup cred n) of
  (Del, Omit, Nothing, Nothing) -> ans
  (Del, Omit, Nothing, Just _) -> ans
  (Del, Omit, Just _, Nothing) -> ans
  (Del, Omit, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Del, Edit _, Nothing, Nothing) -> ans
  (Del, Edit _, Nothing, Just _) -> ans
  (Del, Edit _, Just _, Nothing) -> ans
  (Del, Edit _, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Write _, Omit, Nothing, Nothing) -> ans
  (Write _, Omit, Nothing, Just _) -> ans
  (Write _, Omit, Just _, Nothing) -> ans
  (Write _, Omit, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Write c1, Edit r1, Nothing, Nothing) ->
    insertC r1 (Comb c1) ans
  (Write c1, Edit r1, Nothing, Just _) ->
    insertC r1 (Comb c1) ans
  (Write c1, Edit r1, Just _, Nothing) -> insertC r1 (Comb c1) ans
  (Write c1, Edit r1, Just (Coin c2), Just r2) ->
    insertC r1 (Comb c1) (insertC r2 (Comb (DiffCoin (-c2))) ans)
  (Comb _, Omit, Nothing, Nothing) -> ans
  (Comb _, Omit, Nothing, Just _) -> ans
  (Comb _, Omit, Just _, Nothing) -> ans
  (Comb _, Omit, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Comb c1, Edit r1, Nothing, Nothing) ->
    insertC r1 (Comb c1) ans
  (Comb c1, Edit r1, Nothing, Just _) -> insertC r1 (Comb c1) ans
  (Comb (DiffCoin c1), Edit r1, Just (Coin c2), Nothing) ->
    insertC r1 (Comb (DiffCoin (c1 + c2))) ans
  (Comb (DiffCoin c3), Edit r1, Just (Coin c2), Just r2) ->
    insertC r1 (Comb (DiffCoin (c3 + c2))) (insertC r2 (Comb (DiffCoin (-c2))) ans)

changeDn ::
  (Show cred, Ord cred, Ord drep, Show drep) =>
  Map cred Coin ->
  Map cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  BinaryRngD drep ->
  Map drep (MonoidRngD (Diff Coin))
changeDn m n ans cred dd = case try cred (dd, Map.lookup cred m, Map.lookup cred n) of
  (Omit, Nothing, Nothing) -> ans
  (Omit, Nothing, Just _) -> ans
  (Omit, Just _, Nothing) -> ans
  (Omit, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Edit _, Nothing, Nothing) -> ans
  (Edit _, Nothing, Just _) -> ans
  (Edit r1, Just (Coin c2), Nothing) ->
    insertC r1 (Comb (DiffCoin c2)) ans
  (Edit r1, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) (insertC r1 (Comb (DiffCoin c2)) ans)
-}
-- ======================================================

{-
-- | A stub type, until we decide what a DRep is.
newtype DRep era = DRep Integer
  deriving (Eq, Ord, Show)

deriving newtype instance NFData (DRep era)

instance (Arbitrary (DRep era)) where
  arbitrary = DRep <$> resize 5000 arbitrary
-}

instance (Arbitrary (Diff Coin)) where
  arbitrary = DiffCoin <$> arbitrary

-- ==========================================================
-- Property tests, testing the correctness of the approach.

main :: IO ()
main =
  defaultMain $
    testGroup
      "ILC"
      [ testProperty
          "agg' is derivative of agg"
          ( withMaxSuccess 1000 $
              plusBinary agg agg' (arbitrary @(MonoidMap Int Coin)) arbitrary (arbitrary @(Map Int (KeyHash 'Voting Standard))) arbitrary
          )
      , testProperty "credDistrFromUtxo' is derivative of credDistrFromUtxo" $
          withMaxSuccess 1000 $
            plusUnary @(Map Int (TxOut TT)) credDistrFromUtxo credDistrFromUtxo'
      , testProperty "ptrDistrFromUtxo' is derivative of ptrDistrFromUtxo" $
          withMaxSuccess 1000 $
            plusUnary @(Map Int (TxOut TT)) ptrDistrFromUtxo ptrDistrFromUtxo'
      ]

-- ===============================================================
-- Now we explore a data structure that encapsulates two things.
-- 1) Roots that are easy to update (usually with Log time costs)
-- 2) Dependencies, which are expensive to update (usually with N*Log time costs)
-- Dependencies depend on the Roots, and any change to a root, means the
-- Dependencies have to be recomputed. This data structure is just an
-- exploration of how we may proceed, with enough detail that we can run
-- benchmarks, that demonstrate that using incremental computation,
-- we can compute both Roots and Dependencies cheaply.

type Cred era = Credential 'Staking (EraCrypto era)
type Pool era = KeyHash 'StakePool (EraCrypto era)
type DRep era = KeyHash 'Voting (EraCrypto era)

data IncrementalState era = IS
  { isUtxo :: !(Map (TxIn (EraCrypto era)) (TxOut era))
  , isDelegate :: !(Map (Cred era) (Pool era))
  , isVoteProxy :: !(Map (Cred era) (DRep era))
  , -- \^ These are the 'roots', which are updated directly
    isCredDistr :: !(MonoidMap (Cred era) Coin)
  , isPtrDistr :: !(MonoidMap Ptr Coin)
  , isPoolDistr :: !(MonoidMap (Pool era) Coin)
  , isDRepDistr :: !(MonoidMap (DRep era) Coin)
  }

-- \^ These are the 'dependencies', which are computed from the 'roots'
--   the idea is to compute these incrementally so they always have the
--   most recent results. They automatically get updated every time one
--   or more of the roots change.

deriving instance Era era => Generic (IncrementalState era)
deriving instance (Era era, NFData (TxOut era)) => NFData (IncrementalState era)
deriving instance Show (TxOut era) => Show (IncrementalState era)
deriving instance EraTxOut era => Eq (IncrementalState era)

instance (EraTxOut era, Arbitrary (TxOut era)) => Arbitrary (IncrementalState era) where
  arbitrary = smartIS <$> arbitrary <*> arbitrary <*> arbitrary

-- | This function demonstrates that we can aways compute the Dependencies
--   directly from the roots. This has N*Log cost for each Dependency.
--   Use this only for tests or intializations.
smartIS ::
  forall era.
  (EraTxOut era) =>
  UTxO era ->
  Map (Cred era) (Pool era) ->
  Map (Cred era) (DRep era) ->
  IncrementalState era
smartIS (UTxO u) d v = IS u d v cred' ptr' (computePoolDistr d cred') (computeDRepDistr v cred')
  where
    cred' = credDistrFromUtxo u
    ptr' = ptrDistrFromUtxo u

-- =============================================================
-- Slow Brute force solution

-- | This computes the changes in the 'dependencies' using brute force, that is
--   the non-incremental functions credDistrFromUtxo, ptrDistrFromUtxo , computePoolDistr, and computeDRepDistr, which traverse the entirety
--   of the 'roots' to compute the distribution of stake for each 'dependency'.
slow ::
  forall era.
  (EraTxOut era) =>
  Diff (Map (TxIn (EraCrypto era)) (TxOut era)) ->
  Diff (Map (Cred era) (Pool era)) ->
  Diff (Map (Cred era) (DRep era)) ->
  IncrementalState era ->
  IncrementalState era
slow diff1 diff2 diff3 isState = IS utxo' delmap' votemap' cred' ptr' pool' drep'
  where
    utxo' = applyDiff (isUtxo isState) diff1
    delmap' = applyDiff (isDelegate isState) diff2
    votemap' = applyDiff (isVoteProxy isState) diff3
    cred' = credDistrFromUtxo utxo'
    ptr' = ptrDistrFromUtxo utxo'
    pool' = computePoolDistr delmap' cred'
    drep' = computeDRepDistr votemap' cred'

-- =======================================================
-- functions that compute the dependencies from the roots

-- | Polymorphic function, that can be used at particular types to compute
--   both computePoolDistr and computeDRepDistr. The first map assigns 'delegates'(k2) to keys(k1). The second
--   map assigns 'stake'(Coin) to keys(k1). This agggregates the 'stake'(Coin) for
--   each 'delegate'(k2). For the two uses 'delegates' are StakePools and Voting DReps.
--   This is just a slight refactoring of 'agg' above
f0 ::
  (Ord k1, Ord k2) =>
  Map k1 k2 ->
  MonoidMap k1 Coin ->
  MonoidMap k2 Coin
f0 cd (MM cc) = MM $ Map.foldlWithKey' accum Map.empty cc
  where
    accum ans cred coin =
      case Map.lookup cred cd of
        Just khash -> Map.insertWith (<>) khash coin ans
        Nothing -> ans

-- | Aggregates the UTxO for each staking credential.
credDistrFromUtxo :: (EraTxOut era) => Map k (TxOut era) -> MonoidMap (Cred era) Coin
credDistrFromUtxo m = MM $ Map.foldl' accum Map.empty m
  where
    accum ans txout =
      let coin = txout ^. coinTxOutL
       in case txout ^. addrTxOutL of
            Addr _ _ (StakeRefBase hk) -> Map.insertWith (<>) hk coin ans
            _ -> ans

-- | Aggregates the UTxO for each Ptr credential.
ptrDistrFromUtxo :: (EraTxOut era) => Map k (TxOut era) -> MonoidMap Ptr Coin
ptrDistrFromUtxo m = MM $ Map.foldl' accum Map.empty m
  where
    accum ans txout =
      let coin = txout ^. coinTxOutL
       in case txout ^. addrTxOutL of
            Addr _ _ (StakeRefPtr p) -> Map.insertWith (<>) p coin ans
            _ -> ans

computePoolDistr ::
  Map (Credential 'Staking c) (KeyHash 'StakePool c) ->
  MonoidMap (Credential 'Staking c) Coin ->
  MonoidMap (KeyHash 'StakePool c) Coin
computePoolDistr = f0

computeDRepDistr ::
  Map (Credential 'Staking c) (KeyHash 'Voting c) ->
  MonoidMap (Credential 'Staking c) Coin ->
  MonoidMap (KeyHash 'Voting c) Coin
computeDRepDistr = f0

-- =============================================================
-- Fast and efficient 'update' using incremental techniques

-- | This computes the changes in the 'dependencies' using the incremental
--   functions credDistrFromUtxo', ptrDistrFromUtxo' , computePoolDistr' , computeDRepDistr' which are the ILC derivatives of the
--   brute force functions credDistrFromUtxo, ptrDistrFromUtxo , computePoolDistr, and computeDRepDistr. We expect this to orders of
--   magnitude faster the 'slow'
update ::
  forall era.
  (EraTxOut era) =>
  Diff (Map (TxIn (EraCrypto era)) (TxOut era)) ->
  Diff (Map (Cred era) (Pool era)) ->
  Diff (Map (Cred era) (DRep era)) ->
  IncrementalState era ->
  IncrementalState era
update diff1 diff2 diff3 isState = IS utxo' del' vote' cred' ptr' pool' drep'
  where
    utxo' = applyDiff (isUtxo isState) diff1
    del' = applyDiff (isDelegate isState) diff2
    vote' = applyDiff (isVoteProxy isState) diff3
    cdiff :: Diff (MonoidMap (Cred era) Coin)
    cdiff = credDistrFromUtxo' (isUtxo isState) diff1
    cred' = applyDiff (isCredDistr isState) cdiff
    ptr' = applyDiff (isPtrDistr isState) (ptrDistrFromUtxo' (isUtxo isState) diff1)
    pool' = applyDiff (isPoolDistr isState) (computePoolDistr' (isDelegate isState) diff2 cred' cdiff)
    drep' = applyDiff (isDRepDistr isState) (computeDRepDistr' (isVoteProxy isState) diff3 cred' cdiff)

-- | The derivative of the polymorhphic function f0.
--   A slight refactoring of the function agg'
f0' ::
  (Show k1, Show k2, Ord k1, Ord k2, MapLike m) =>
  m k1 k2 ->
  Diff (Map k1 k2) ->
  MonoidMap k1 Coin ->
  Diff (MonoidMap k1 Coin) ->
  Diff (MonoidMap k2 Coin)
f0' n (Dn dn) (MM m) (Dm dm) =
  Dm $
    inter3C Map.empty dm dn (changeDm2 m n) (changeDmDn2 m n) (changeDn2 m n)

-- The derivative of credDistrFromUtxo. We make this polymorhic over 'k' so it is more likely
-- we will test the unlikely cases.

credDistrFromUtxo' ::
  (EraTxOut era, Ord k) =>
  Map k (TxOut era) ->
  Diff (Map k (TxOut era)) ->
  Diff (MonoidMap (Cred era) Coin)
credDistrFromUtxo' utxo (Dn changes) = Dm $ Map.foldlWithKey' accum Map.empty changes
  where
    accum ans k Omit = case Map.lookup k utxo of
      Nothing -> ans
      (Just txout) -> case txout ^. addrTxOutL of
        Addr _ _ (StakeRefBase hk) -> insertC hk (Comb (DiffCoin (-n))) ans
          where
            (Coin n) = txout ^. coinTxOutL
        _ -> ans
    accum ans k (Edit txout1) = case Map.lookup k utxo of
      Nothing -> case txout1 ^. addrTxOutL of
        Addr _ _ (StakeRefBase hk) -> insertC hk (Comb (DiffCoin n)) ans
          where
            (Coin n) = txout1 ^. coinTxOutL
        _ -> ans
      -- This case is highly unlikely, as k=TxIn, which is a cryptographic hash
      (Just txout2) -> case (txout1 ^. addrTxOutL, txout2 ^. addrTxOutL) of
        (Addr _ _ (StakeRefBase hk1), Addr _ _ (StakeRefBase hk2)) ->
          insertC hk1 (Comb (DiffCoin n1)) (insertC hk2 (Comb (DiffCoin (-n2))) ans)
          where
            (Coin n1) = txout1 ^. coinTxOutL
            (Coin n2) = txout2 ^. coinTxOutL
        (Addr _ _ (StakeRefBase hk1), _) -> insertC hk1 (Comb (DiffCoin n1)) ans
          where
            (Coin n1) = txout1 ^. coinTxOutL
        (_, Addr _ _ (StakeRefBase hk2)) -> insertC hk2 (Comb (DiffCoin (-n2))) ans
          where
            (Coin n2) = txout2 ^. coinTxOutL
        (_, _) -> ans

-- The derivative of ptrDistrFromUtxo
ptrDistrFromUtxo' ::
  (Ord k, EraTxOut era) =>
  Map k (TxOut era) ->
  Diff (Map k (TxOut era)) ->
  Diff (MonoidMap Ptr Coin)
ptrDistrFromUtxo' utxo (Dn changes) = Dm $ Map.foldlWithKey' accum Map.empty changes
  where
    accum ans k Omit = case Map.lookup k utxo of
      Nothing -> ans
      (Just txout) -> case txout ^. addrTxOutL of
        Addr _ _ (StakeRefPtr p) -> insertC p (Comb (DiffCoin (-n))) ans
          where
            (Coin n) = txout ^. coinTxOutL
        _ -> ans
    accum ans k (Edit txout1) = case Map.lookup k utxo of
      Nothing -> case txout1 ^. addrTxOutL of
        Addr _ _ (StakeRefPtr p) -> insertC p (Comb (DiffCoin n)) ans
          where
            (Coin n) = txout1 ^. coinTxOutL
        _ -> ans
      -- This case is highly unlikely, as k=TxIn, which is a cryptographic hash
      (Just txout2) -> case (txout1 ^. addrTxOutL, txout2 ^. addrTxOutL) of
        (Addr _ _ (StakeRefPtr p1), Addr _ _ (StakeRefPtr p2)) ->
          insertC p1 (Comb (DiffCoin n1)) (insertC p2 (Comb (DiffCoin (-n2))) ans)
          where
            (Coin n1) = txout1 ^. coinTxOutL
            (Coin n2) = txout2 ^. coinTxOutL
        (Addr _ _ (StakeRefPtr p1), _) -> insertC p1 (Comb (DiffCoin n1)) ans
          where
            (Coin n1) = txout1 ^. coinTxOutL
        (_, Addr _ _ (StakeRefPtr p2)) -> insertC p2 (Comb (DiffCoin (-n2))) ans
          where
            (Coin n2) = txout2 ^. coinTxOutL
        (_, _) -> ans

-- The derivative of computePoolDistr (just instantiates f0' at the correct type)
computePoolDistr' ::
  forall c m.
  MapLike m =>
  m (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Diff (Map (Credential 'Staking c) (KeyHash 'StakePool c)) ->
  MonoidMap (Credential 'Staking c) Coin ->
  Diff (MonoidMap (Credential 'Staking c) Coin) ->
  Diff (MonoidMap (KeyHash 'StakePool c) Coin)
computePoolDistr' = f0'

-- The derivative of computeDRepDistr (just instantiates f0' at the correct type)
computeDRepDistr' ::
  forall c.
  Map (Credential 'Staking c) (KeyHash 'Voting c) ->
  Diff (Map (Credential 'Staking c) (KeyHash 'Voting c)) ->
  MonoidMap (Credential 'Staking c) Coin ->
  Diff (MonoidMap (Credential 'Staking c) Coin) ->
  Diff (MonoidMap (KeyHash 'Voting c) Coin)
computeDRepDistr' = f0'

-- =========================================================================
{-
 { isUtxo :: !(Map (TxIn (EraCrypto era)) (TxOut era))
  , isDelegate :: !(Map (Cred era) (Pool era))
  , isVoteProxy :: !(Map (Cred era) (DRep era))
-}

data ILCState era = ILCState
  { ilcCredDistr :: !(MonoidMap (Cred era) Coin)
  , ilcPtrDistr :: !(MonoidMap Ptr Coin)
  , ilcPoolDistr :: !(MonoidMap (Pool era) Coin)
  , ilcDRepDistr :: !(MonoidMap (DRep era) Coin)
  }

utxoL :: Lens' (LedgerState era) (UTxO era)
utxoL = lsUTxOStateL . utxosUtxoL

poolL :: Lens' (LedgerState era) (View (EraCrypto era) (Cred era) (Pool era))
poolL = lsDPStateL . dpsDStateL . dsUnifiedL . umapPool

umapPool :: Lens' (UM.UMap c) (View c (Credential 'Staking c) (KeyHash 'StakePool c))
umapPool = lens Delegations (\_umap (Delegations um) -> um)

drepL :: Lens' (LedgerState era) (View (EraCrypto era) (Cred era) (DRep era))
drepL = lsDPStateL . dpsDStateL . dsUnifiedL . umapD

umapD :: Lens' (UM.UMap c) (View c (Credential 'Staking c) (KeyHash 'Voting c))
umapD = lens Dreps (\_umap (Dreps um) -> um)

ilcL :: Lens' (LedgerState era) (ILCState era)
ilcL = lsDPStateL . undefined

updateILC ::
  forall era.
  EraTxOut era =>
  Diff (Map (TxIn (EraCrypto era)) (TxOut era)) ->
  Diff (View (EraCrypto era) (Cred era) (Pool era)) ->
  Diff (View (EraCrypto era) (Cred era) (DRep era)) ->
  LedgerState era ->
  LedgerState era
updateILC dUtxo dPool dDrep ls =
  ls
    & ilcL .~ (ILCState cred' ptr' pool' drep')
    & utxoL .~ (UTxO utxoNew)
    & poolL .~ delNew
    & drepL .~ voteNew
  where
    UTxO utxo = ls ^. utxoL
    del = ls ^. poolL
    vote = ls ^. drepL
    (ILCState credDistr ptrDistr poolDistr drepDistr) = ls ^. ilcL
    utxoNew = utxo `applyDiff` dUtxo
    delNew = del `applyDiff` dPool
    voteNew = vote `applyDiff` dDrep
    cdiff :: Diff (MonoidMap (Cred era) Coin)
    cdiff = credDistrFromUtxo' utxo dUtxo
    cred' = credDistr `applyDiff` cdiff
    ptr' = ptrDistr `applyDiff` (ptrDistrFromUtxo' utxo dUtxo)
    pool' = poolDistr `applyDiff` (computePoolDistr'2 del dPool cred' cdiff)
    drep' = drepDistr `applyDiff` (computeDRepDistr'2 vote dDrep cred' cdiff)

-- The derivative of computePoolDistr adjusted for the fact that the the first
-- arg is a View, rather than a Map.
computePoolDistr'2 ::
  View c (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Diff (View c (Credential 'Staking c) (KeyHash 'StakePool c)) ->
  MonoidMap (Credential 'Staking c) Coin ->
  Diff (MonoidMap (Credential 'Staking c) Coin) ->
  Diff (MonoidMap (KeyHash 'StakePool c) Coin)
computePoolDistr'2 n (Dl dn) (MM m) (Dm dm) =
  Dm $
    inter3C Map.empty dm dn (changeDm2 m n) (changeDmDn2 m n) (changeDn2 m n)

-- The derivative of computeDRepDistr adjusted for the fact that the the first
-- arg is a View, rather than a Map.
computeDRepDistr'2 ::
  View c (Credential 'Staking c) (KeyHash 'Voting c) ->
  Diff (View c (Credential 'Staking c) (KeyHash 'Voting c)) ->
  MonoidMap (Credential 'Staking c) Coin ->
  Diff (MonoidMap (Credential 'Staking c) Coin) ->
  Diff (MonoidMap (KeyHash 'Voting c) Coin)
computeDRepDistr'2 n (Dl dn) (MM m) (Dm dm) =
  Dm $
    inter3C Map.empty dm dn (changeDm2 m n) (changeDmDn2 m n) (changeDn2 m n)

-- ======================================

instance Ord k => ILC (UM.View c k v) where
  newtype Diff (UM.View c k v) = Dl (Map k (BinaryRngD v))
  applyDiff view (Dl changes) = Map.foldlWithKey' accum view changes
    where
      accum ans k Omit = deleteLike k ans
      accum ans k (Edit keyhash) = insertLike k keyhash ans
  zero = Dl Map.empty
  extend (Dl x) (Dl y) = Dl (Map.unionWith (<>) x y)

instance ILC (UM.UMap c) where
  newtype Diff (UM.UMap c) = Du (Map (Credential 'Staking c) (BinaryRngD (KeyHash 'StakePool c)))
  applyDiff umap (Du changes) = Map.foldlWithKey' accum umap changes
    where
      accum ans k Omit = UM.delete k (UM.Delegations ans)
      accum ans k (Edit keyhash) = UM.insert k keyhash (UM.Delegations ans)
  zero = Du Map.empty
  extend (Du x) (Du y) = Du (Map.unionWith (<>) x y)

-- =========================================================================

changeDm2 ::
  (Show cred, Ord cred, Ord drep, Show drep, MapLike mapT, MapLike mapS) =>
  mapT cred Coin ->
  -- | either a Map or a View that behaves like a map.
  mapS cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  MonoidRngD (Diff Coin) ->
  Map drep (MonoidRngD (Diff Coin))
changeDm2 m n ans cred dcoin = case try cred (dcoin, lookupLike cred m, lookupLike cred n) of
  (Del, Nothing, Nothing) -> ans
  (Del, Nothing, Just _) -> ans
  (Del, Just _, Nothing) -> ans
  (Del, Just (Coin c2), Just r2) -> insertC r2 (Comb (DiffCoin (-c2))) ans
  (Write _, Nothing, Nothing) -> ans
  (Write c1, Nothing, Just r2) -> insertC r2 (Comb c1) ans
  (Write _, Just _, Nothing) -> ans
  (Write (DiffCoin c1), Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (c1 - c2))) ans
  (Comb _, Nothing, Nothing) -> ans
  (Comb c1, Nothing, Just r2) -> insertC r2 (Comb c1) ans
  (Comb _, Just _, Nothing) -> ans
  (Comb (DiffCoin c1), Just _, Just r2) -> insertC r2 (Comb (DiffCoin c1)) ans

changeDmDn2 ::
  forall mapT mapS cred drep.
  (Show cred, Ord cred, Show drep, Ord drep, MapLike mapT, MapLike mapS) =>
  mapT cred Coin ->
  mapS cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  (MonoidRngD (Diff Coin), BinaryRngD drep) ->
  Map drep (MonoidRngD (Diff Coin))
changeDmDn2 m n ans cred (dcoin, drep) =
  case try cred (dcoin, drep, lookupLike cred m, lookupLike cred n) of
    (Del, Omit, Nothing, Nothing) -> ans
    (Del, Omit, Nothing, Just _) -> ans
    (Del, Omit, Just _, Nothing) -> ans
    (Del, Omit, Just (Coin c2), Just r2) ->
      insertC r2 (Comb (DiffCoin (-c2))) ans
    (Del, Edit _, Nothing, Nothing) -> ans
    (Del, Edit _, Nothing, Just _) -> ans
    (Del, Edit _, Just _, Nothing) -> ans
    (Del, Edit _, Just (Coin c2), Just r2) ->
      insertC r2 (Comb (DiffCoin (-c2))) ans
    (Write _, Omit, Nothing, Nothing) -> ans
    (Write _, Omit, Nothing, Just _) -> ans
    (Write _, Omit, Just _, Nothing) -> ans
    (Write _, Omit, Just (Coin c2), Just r2) ->
      insertC r2 (Comb (DiffCoin (-c2))) ans
    (Write c1, Edit r1, Nothing, Nothing) ->
      insertC r1 (Comb c1) ans
    (Write c1, Edit r1, Nothing, Just _) ->
      insertC r1 (Comb c1) ans
    (Write c1, Edit r1, Just _, Nothing) -> insertC r1 (Comb c1) ans
    (Write c1, Edit r1, Just (Coin c2), Just r2) ->
      insertC r1 (Comb c1) (insertC r2 (Comb (DiffCoin (-c2))) ans)
    (Comb _, Omit, Nothing, Nothing) -> ans
    (Comb _, Omit, Nothing, Just _) -> ans
    (Comb _, Omit, Just _, Nothing) -> ans
    (Comb _, Omit, Just (Coin c2), Just r2) ->
      insertC r2 (Comb (DiffCoin (-c2))) ans
    (Comb c1, Edit r1, Nothing, Nothing) ->
      insertC r1 (Comb c1) ans
    (Comb c1, Edit r1, Nothing, Just _) -> insertC r1 (Comb c1) ans
    (Comb (DiffCoin c1), Edit r1, Just (Coin c2), Nothing) ->
      insertC r1 (Comb (DiffCoin (c1 + c2))) ans
    (Comb (DiffCoin c3), Edit r1, Just (Coin c2), Just r2) ->
      insertC r1 (Comb (DiffCoin (c3 + c2))) (insertC r2 (Comb (DiffCoin (-c2))) ans)

changeDn2 ::
  (Show cred, Ord cred, Ord drep, Show drep, MapLike mapT, MapLike mapS) =>
  mapT cred Coin ->
  mapS cred drep ->
  Map drep (MonoidRngD (Diff Coin)) ->
  cred ->
  BinaryRngD drep ->
  Map drep (MonoidRngD (Diff Coin))
changeDn2 m n ans cred dd = case try cred (dd, lookupLike cred m, lookupLike cred n) of
  (Omit, Nothing, Nothing) -> ans
  (Omit, Nothing, Just _) -> ans
  (Omit, Just _, Nothing) -> ans
  (Omit, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) ans
  (Edit _, Nothing, Nothing) -> ans
  (Edit _, Nothing, Just _) -> ans
  (Edit r1, Just (Coin c2), Nothing) ->
    insertC r1 (Comb (DiffCoin c2)) ans
  (Edit r1, Just (Coin c2), Just r2) ->
    insertC r2 (Comb (DiffCoin (-c2))) (insertC r1 (Comb (DiffCoin c2)) ans)
