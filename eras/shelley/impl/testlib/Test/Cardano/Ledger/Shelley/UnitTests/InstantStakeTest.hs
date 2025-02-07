{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest (spec) where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (EraTxOut, mkCoinTxOut)
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.Shelley.LedgerState (dsUnifiedL, psStakePoolParamsL)
import Cardano.Ledger.State
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.UMap (RDPair (..), rdRewardCoin)
import qualified Cardano.Ledger.UMap as UM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MonoTuple (TupleN)
import qualified Data.VMap as VMap
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)

ppIdL :: Lens' PoolParams (KeyHash 'StakePool)
ppIdL = lens ppId (\x y -> x {ppId = y})

arbitraryLens :: Arbitrary a => Lens' a b -> b -> Gen a
arbitraryLens l b = (l .~ b) <$> arbitrary

-- ===========================================

instantStakeIncludesRewards :: forall era. (EraTxOut era, EraStake era) => Gen Property
instantStakeIncludesRewards = do
  (tom, john, ann, ron, mary) <- arbitrary @(TupleN 5 (Credential 'Staking))
  (tomPay, johnPay, annPay, ronPay) <- arbitrary @(TupleN 4 (Credential 'Payment))

  (pool1, pool2) <- arbitrary @(TupleN 2 (KeyHash 'StakePool))
  pool1Params <- arbitraryLens ppIdL pool1
  pool2Params <- arbitraryLens ppIdL pool2

  (tomRD, johnRD, annRD, ronRD, maryRD) <- arbitrary @(TupleN 5 RDPair)

  let tomAddr = mkAddr tomPay StakeRefNull
      johnAddr = mkAddr johnPay john
      annAddr = mkAddr annPay ann
      ronAddr = mkAddr ronPay ron
      -- maryAddr is omitted on purpose. Mary will not have a UTxO entry

      rewards :: Map (Credential 'Staking) RDPair
      rewards =
        Map.fromList
          [ (tom, tomRD)
          , (john, johnRD)
          , (ann, annRD)
          , (ron, ronRD)
          , (mary, maryRD)
          ]

      delegations :: Map (Credential 'Staking) (KeyHash 'StakePool)
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

  (tomTxIn1, tomTxIn2, johnTxIn, annTxIn, ronTxIn) <- arbitrary @(TupleN 5 TxIn)

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

    instantStake = addInstantStake utxo1 mempty
    umap = UM.unify rewards Map.empty delegations Map.empty
    poolparamMap = Map.fromList [(pool1, pool1Params), (pool2, pool2Params)]
  -- We can either use an emptyDstate with just the umap, like this
  -- dState = (emptyDState {dsUnified = umap}) :: DState era
  -- Or an arbitrary one, where we overwrite the umap, with the one we need.
  dState <- arbitraryLens dsUnifiedL umap
  pState <- arbitraryLens psStakePoolParamsL poolparamMap
  let snapShot = snapShotFromInstantStake instantStake dState pState
      computedStakeDistr = Map.map fromCompact (VMap.toMap (unStake (ssStake snapShot)))

      expectedStakeDistr :: Map (Credential 'Staking) Coin
      expectedStakeDistr =
        Map.fromList $
          [ (ann, rdRewardCoin annRD <> annCoin)
          , (ron, rdRewardCoin ronRD <> ronCoin)
          , (john, rdRewardCoin johnRD <> johnCoin)
          ]
            ++ [ stake
               | stake@(_, reward) <-
                  [ (tom, rdRewardCoin tomRD) -- tom uxtxoCoin cab be zero because his address has StakeRefNull
                  , (mary, rdRewardCoin maryRD) -- mary uxtxoCoin can be zero because she has no UtxO entry
                  ]
               , reward /= Coin 0 -- We need to filter out zero rewards from instant stake
               ]

  pure (computedStakeDistr === expectedStakeDistr)

spec :: forall era. (EraTxOut era, EraStake era) => Spec
spec = prop "InstantStakeIncludesRewards" (instantStakeIncludesRewards @era)
