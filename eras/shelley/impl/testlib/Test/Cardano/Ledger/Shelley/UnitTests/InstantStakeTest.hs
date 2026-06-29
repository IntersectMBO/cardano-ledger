{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Shelley.UnitTests.InstantStakeTest (spec) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (CompactForm, fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential, StakeReference (..))
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.TxIn (TxIn)
import Data.Default (def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MonoTuple (TupleN)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import Lens.Micro
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Shelley.Era
import Test.Cardano.Ledger.Shelley.ImpTest

sppIdL :: Lens' StakePoolParams (KeyHash StakePool)
sppIdL = lens sppId (\x y -> x {sppId = y})

-- | Generate an arbitrary value and overwrite the specified value using the supplied lens.
arbitraryLens :: Arbitrary a => Lens' a b -> b -> Gen a
arbitraryLens l b = (l .~ b) <$> arbitrary

-- ===========================================

instantStakeIncludesRewards :: forall era. ShelleyEraImp era => Gen Property
instantStakeIncludesRewards = do
  (pool1, pool2) <- arbitrary @(TupleN 2 StakePoolParams)
  let
    poolId1 = pool1 ^. sppIdL
    poolId2 = pool2 ^. sppIdL
    poolParamsMap = Map.fromList [(poolId1, pool1), (poolId2, pool2)]
  pState <- arbitraryLens psStakePoolsL $ mkStakePoolState mempty mempty <$> poolParamsMap
  let
    initCertState :: CertState era
    initCertState = def & certPStateL .~ pState
    registerAccount poolId certState = do
      stakingCredential <- arbitrary
      ptr <- arbitrary
      deposit <- arbitrary
      balance <- arbitrary
      let accounts :: Accounts era
          accounts =
            addToBalanceAccounts (Map.singleton stakingCredential balance) $
              registerTestAccount stakingCredential (Just ptr) deposit (Just poolId) Nothing $
                certState ^. certDStateL . accountsL
          certState' =
            certState
              & certDStateL . accountsL .~ accounts
              & certPStateL . psStakePoolsL
                %~ Map.adjust (spsDelegatorsL %~ Set.insert stakingCredential) poolId
      pure (stakingCredential, balance, certState')
  (tom, tomBalance, certState0) <- registerAccount poolId1 initCertState
  (john, johnBalance, certState1) <- registerAccount poolId2 certState0
  (ann, annBalance, certState2) <- registerAccount poolId2 certState1
  (ron, ronBalance, certState3) <- registerAccount poolId1 certState2
  (mary, maryBalance, certState) <- registerAccount poolId2 certState3
  (tomPay, johnPay, annPay, ronPay) <- arbitrary @(TupleN 4 (Credential Payment))

  let tomAddr = mkAddr tomPay StakeRefNull
      johnAddr = mkAddr johnPay john
      annAddr = mkAddr annPay ann
      ronAddr = mkAddr ronPay ron
  -- maryAddr is omitted on purpose. Mary will not have a UTxO entry

  -- Again mary is not included, because she will not have an UTxO entry, but tom will have 2
  (tomCoin1, tomCoin2, johnCoin, annCoin, ronCoin) <- arbitrary @(TupleN 5 (CompactForm Coin))

  (tomTxIn1, tomTxIn2, johnTxIn, annTxIn, ronTxIn) <- arbitrary @(TupleN 5 TxIn)

  let
    -- Each wallet (except mary) has one or more UTxO entries
    -- Since tom uses a StakeRefNull those entries will not be distributed
    utxo1 =
      UTxO @era
        ( Map.fromList
            [ (tomTxIn1, mkCoinTxOut tomAddr $ fromCompact tomCoin1) -- Not distrubuted, see tomAddr
            , (tomTxIn2, mkCoinTxOut tomAddr $ fromCompact tomCoin2) -- Not distributed, see tomAddr
            , (annTxIn, mkCoinTxOut annAddr $ fromCompact annCoin)
            , (ronTxIn, mkCoinTxOut ronAddr $ fromCompact ronCoin)
            , (johnTxIn, mkCoinTxOut johnAddr $ fromCompact johnCoin)
            -- Note Mary does not have a UTxO entry, but her rewards are still counted
            ]
        )
    instantStake = addInstantStake utxo1 mempty
  let snapShot =
        snapShotFromInstantStake instantStake (certState ^. certDStateL) (certState ^. certPStateL)
      computedStakeDistr = Map.map (unNonZero . swdStake) $ VMap.toMap $ unActiveStake $ ssActiveStake snapShot

      expectedStakeDistr :: Map (Credential Staking) (CompactForm Coin)
      expectedStakeDistr =
        Map.fromList $
          [ stake
          | stake@(_, balance) <-
              [ (ann, annBalance <> annCoin)
              , (ron, ronBalance <> ronCoin)
              , (john, johnBalance <> johnCoin)
              , (tom, tomBalance)
              , (mary, maryBalance)
              ]
          , -- We need to filter out zero rewards from instant stake, since all coins are
          -- generated at random, stake for any one of the accounts can be zero
          balance /= mempty
          ]

  pure (computedStakeDistr === expectedStakeDistr)

spec :: forall era. ShelleyEraImp era => Spec
spec = prop "InstantStakeIncludesRewards" (instantStakeIncludesRewards @era)
