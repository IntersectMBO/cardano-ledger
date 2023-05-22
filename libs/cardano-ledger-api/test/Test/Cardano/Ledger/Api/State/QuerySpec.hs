
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Api.State.QuerySpec (spec) where

import Cardano.Ledger.Api.State.Query
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.UMap (UMap)
import Test.Cardano.Ledger.Api.State.Query
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (genValidUMapWithCreds)

spec :: Spec
spec = do
  describe "GetFilteredDelegationsAndRewardAccounts" $ do
    prop "filterStakePoolDelegsAndRewards same as getFilteredDelegationsAndRewardAccounts" $
      forAll genValidUMapWithCreds $ \(umap :: UMap StandardCrypto, creds) ->
        filterStakePoolDelegsAndRewards umap creds
          `shouldBe` getFilteredDelegationsAndRewardAccounts umap creds
