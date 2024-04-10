{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.UnitTests.IncrementalStakeTest (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.BaseTypes (Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (Era (..), EraTxOut, emptyPParams, mkCoinTxOut)
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.EpochBoundary (SnapShot (..), Stake (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.LedgerState (
  IncrementalStake (..),
  dsUnifiedL,
  incrementalStakeDistr,
  psStakePoolParamsL,
  updateStakeDistribution,
 )
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UMap (RDPair (..), rdRewardCoin)
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MonoTuple (TupleN)
import qualified Data.VMap as VMap
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

ppIdL :: Lens' (PoolParams c) (KeyHash 'StakePool c)
ppIdL = lens ppId (\x y -> x {ppId = y})

address :: Credential 'Payment c -> Maybe (Credential 'Staking c) -> Addr c
address pc Nothing = Addr Testnet pc StakeRefNull
address pc (Just sc) = Addr Testnet pc (StakeRefBase sc)

arbitraryLens :: Arbitrary a => Lens' a b -> b -> Gen a
arbitraryLens l b = do a <- arbitrary; pure (a & l .~ b)

-- ===========================================

stakeDistrIncludesRewards :: forall era. EraTxOut era => Gen Property
stakeDistrIncludesRewards = do
  (tom, john, ann, ron, mary) <- arbitrary @(TupleN 5 (Credential 'Staking (EraCrypto era)))
  (tomPay, johnPay, annPay, ronPay) <- arbitrary @(TupleN 4 (Credential 'Payment (EraCrypto era)))

  (pool1, pool2) <- arbitrary @(TupleN 2 (KeyHash 'StakePool (EraCrypto era)))
  pool1Params <- arbitraryLens ppIdL pool1
  pool2Params <- arbitraryLens ppIdL pool2

  (tomRD, johnRD, annRD, ronRD, maryRD) <- arbitrary @(TupleN 5 RDPair)

  let tomAddr = address tomPay Nothing -- Nothing means tomAddr does not have a StakeReference
      johnAddr = address johnPay (Just john)
      annAddr = address annPay (Just ann)
      ronAddr = address ronPay (Just ron)
      -- maryAddr is omitted on purpose. Mary will not have a UTxO entry

      rewards :: Map (Credential 'Staking (EraCrypto era)) RDPair
      rewards =
        Map.fromList
          [ (tom, tomRD)
          , (john, johnRD)
          , (ann, annRD)
          , (ron, ronRD)
          , (mary, maryRD)
          ]

      delegations :: Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era))
      delegations =
        Map.fromList
          [ (tom, pool1) -- since every one is delegated
          , (ann, pool2) -- no one's stake should be left out
          , (ron, pool1)
          , (john, pool2)
          , (mary, pool2)
          ]

  -- Again mary is not included, because she will not have an UTxO entry, but tom will have 2
  (tomCoin1, tomCoin2, johnCoin, annCoin, ronCoin) <- arbitrary @(TupleN 5 Coin)

  (tomTxIn1, tomTxIn2, johnTxIn, annTxIn, ronTxIn) <- arbitrary @(TupleN 5 (TxIn (EraCrypto era)))

  let
    -- Each wallet (except mary) has one or more UTxO entries
    -- Since tom uses a StakeRefNull those entries will not be distributed
    utxo1 =
      UTxO @era
        ( Map.fromList
            [ (tomTxIn1, mkCoinTxOut tomAddr tomCoin1) -- Not distrubuted, see tomAddr
            , (tomTxIn2, mkCoinTxOut tomAddr tomCoin2) -- Not distributed, see tomAddr
            , (annTxIn, mkCoinTxOut annAddr annCoin)
            , (ronTxIn, mkCoinTxOut ronAddr ronCoin)
            , (johnTxIn, mkCoinTxOut johnAddr johnCoin)
            -- Note Mary does not have a UTxO entry, but her rewards are still counted
            ]
        )

    pparams = emptyPParams @era

    incrementalStake = updateStakeDistribution pparams (IStake mempty mempty) mempty utxo1
    umap = UM.unify rewards Map.empty delegations Map.empty
    poolparamMap = Map.fromList [(pool1, pool1Params), (pool2, pool2Params)]
  -- We can either use an emptyDstate with just the umap, like this
  -- dState = (emptyDState {dsUnified = umap}) :: DState era
  -- Or an arbitrary one, where we overwrite the umap, with the one we need.
  dState <- arbitraryLens dsUnifiedL umap
  pState <- arbitraryLens psStakePoolParamsL poolparamMap
  let computedStakeDistr = Map.map fromCompact (VMap.toMap (unStake (ssStake snap)))
        where
          snap =
            incrementalStakeDistr -- This computes the actual Incremental Stake
              pparams
              incrementalStake
              dState
              pState

      expectedStakeDistr :: Map (Credential 'Staking (EraCrypto era)) Coin
      expectedStakeDistr =
        Map.fromList -- Coin Part is (rdRewardCoin <> utxoCoin)
          [ (tom, rdRewardCoin tomRD <> Coin 0) -- tom uxtxoCoin is zero because his address has StakeRefNull
          , (ann, rdRewardCoin annRD <> annCoin)
          , (ron, rdRewardCoin ronRD <> ronCoin)
          , (john, rdRewardCoin johnRD <> johnCoin)
          , (mary, rdRewardCoin maryRD <> Coin 0) -- mary uxtxoCoin is zero because she has no UtxO entry
          ]

  pure $ (computedStakeDistr === expectedStakeDistr)

spec :: forall era. EraTxOut era => Spec
spec = prop "StakeDistrIncludesRewards" (stakeDistrIncludesRewards @era)
