{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Bench.Cardano.Ledger.Incremental where

import Cardano.Ledger.Core (EraTxOut (..), TxOut)
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.UTxO (UTxO (..))
import Criterion
import Data.Incremental (Diff (Dn), ILC (..), MonoidMap (..), unMM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.Cardano.Data (genBinaryRngD)
import Test.Cardano.Ledger.Babbage.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Generic.Proof (Evidence (..), Proof (..), ShelleyEra, Standard)
import Test.Cardano.Ledger.Incremental (
  Cred,
  DRep (..),
  IncrementalState (..),
  Pool,
  f1,
  slow,
  smartIS,
  update,
 )
import Test.QuickCheck

-- ============================

type TT = ShelleyEra Standard

-- ===================================================================
-- Arbitrary instances for tests and benchmarks
-- ======================================================================================

genUTxO ::
  forall era proxy.
  (EraTxOut era, Arbitrary (TxOut era)) =>
  proxy era ->
  Int ->
  Gen (Map (TxIn (EraCrypto era)) (TxOut era))
genUTxO _p n =
  Map.fromList
    <$> vectorOf
      n
      ( (,)
          <$> arbitrary @(TxIn (EraCrypto era))
          <*> arbitrary @(TxOut era)
      )

genDel :: (Ord cred, Era era) => proxy era -> Int -> Gen cred -> Gen (Map cred (Pool era))
genDel _p n gcred = Map.fromList <$> vectorOf n ((,) <$> gcred <*> arbitrary)

genVote :: (Ord cred) => proxy era -> Int -> Gen cred -> Gen (Map cred (DRep era))
genVote _p n gcred = Map.fromList <$> vectorOf n ((,) <$> gcred <*> (DRep <$> choose (1, 1000)))

genKey :: Map k v -> Gen k
genKey m = do
  i <- chooseInt (0, Map.size m - 1)
  pure (fst (Map.elemAt i m))

-- | Generate a UTxO Diff with approximate size 'n', The actual
--   size does not matter but the ord of magnitude does.
genUtxoDiff ::
  forall era proxy.
  (Arbitrary (TxOut era)) =>
  proxy era ->
  Int ->
  Gen (TxIn (EraCrypto era)) ->
  Gen (Diff (Map (TxIn (EraCrypto era)) (TxOut era)))
genUtxoDiff _p n genTxIn =
  Dn . Map.fromList
    <$> vectorOf n ((,) <$> genTxIn <*> genBinaryRngD (arbitrary @(TxOut era)))

-- | Generate a Map Diff with approximate size 'n', The actual
--   size does not matter but the ord of magnitude does.
genMapDiff :: forall era k v proxy. Ord k => proxy era -> Int -> Gen k -> Gen v -> Gen (Diff (Map k v))
genMapDiff _p n genK genVal =
  Dn . Map.fromList <$> vectorOf n ((,) <$> genK <*> genBinaryRngD genVal)

-- ==========================================================

-- | An IO action used to precompute inputs for the benchmarks.
setupEnv ::
  IO
    ( IncrementalState TT
    , Diff (Map (TxIn Standard) (TxOut TT))
    , Diff (Map (Cred TT) (Pool TT))
    , Diff (Map (Cred TT) (DRep TT))
    )
setupEnv = do
  let p = (Shelley Standard)
  utxo <- generate (genUTxO p 1000000)
  let MM creds = f1 utxo
      gcred = genKey creds
  vote <- generate $ genVote p 3000 gcred
  delegate <- generate $ genDel p 3000 gcred
  let is = (smartIS (UTxO utxo) delegate vote)
  putStrLn ("isUtxo = " ++ show (Map.size (isUtxo is)))
  putStrLn ("isDelegate = " ++ show (Map.size (isDelegate is)))
  putStrLn ("isVoteProxy = " ++ show (Map.size (isVoteProxy is)))
  putStrLn ("isCredDistr = " ++ show (Map.size (unMM (isCredDistr is))))
  putStrLn ("isPtrDistr = " ++ show (Map.size (unMM (isPtrDistr is))))
  putStrLn ("isPoolDistr = " ++ show (Map.size (unMM (isPoolDistr is))))
  putStrLn ("isDRepDistr = " ++ show (Map.size (unMM (isDRepDistr is))))
  putStrLn ("creds size = " ++ show (Map.size creds))
  diff1 <- generate $ genUtxoDiff p 3 (genKey utxo)
  diff2 <- generate $ genMapDiff p 3 gcred (arbitrary @(Pool (ShelleyEra Standard)))
  diff3 <- generate $ genMapDiff p 3 gcred (DRep <$> choose (1, 3000))
  pure (is, diff1, diff2, diff3)

-- | The benchmark
slowVsIncremental :: Benchmark
slowVsIncremental =
  env setupEnv $ \ ~(is, diff1, diff2, diff3) ->
    bgroup
      "main"
      [ bench "just UTxO" $ whnf (justUTxO diff1) is
      , bench "slow" $ whnf (slow diff1 diff2 diff3) is
      , bench "incremental" $ whnf (update diff1 diff2 diff3) is
      , bench "incremental on UTxO only" $ whnf (update diff1 (Dn Map.empty) (Dn Map.empty)) is
      ]

justUTxO ::
  Diff (Map (TxIn (EraCrypto era)) (TxOut era)) ->
  IncrementalState era ->
  IncrementalState era
justUTxO diff1 isState =
  isState {isUtxo = (applyDiff (isUtxo isState) diff1)}
