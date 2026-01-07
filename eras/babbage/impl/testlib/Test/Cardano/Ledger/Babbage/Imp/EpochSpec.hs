{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Imp.EpochSpec (babbageEraSpecificSpec) where

import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential
import Control.Monad.Writer (listen)
import Test.Cardano.Ledger.Babbage.ImpTest
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Imp.Common (mkAddr)

babbageEraSpecificSpec ::
  forall era.
  AlonzoEraImp era =>
  SpecWith (ImpInit (LedgerSpec era))
babbageEraSpecificSpec = do
  it "Pool to pool member rewards" $ do
    pools@[p1, p2] <- replicateM 2 freshKeyHash
    screds@[s1, s2, s3] <- replicateM 3 (KeyHashObj <$> freshKeyHash)
    pcreds <- replicateM 3 (freshKeyHash @Payment)
    let addrs = zipWith mkAddr pcreds screds

    mapM_ (`sendCoinTo_` Coin 9_000_000) addrs
    mapM_ registerPool pools
    mapM_ registerStakeCredential screds

    delegateStake s1 p1
    delegateStake s2 p2
    delegateStake s3 p1

    replicateM_ 3 $ do
      evs <- listen passEpoch
      logDoc $ "EVs: " <> ansiExpr evs

    mapM_ (`sendCoinTo_` Coin 90_000_000) addrs

    replicateM_ 3 $ do
      evs <- listen passEpoch
      logDoc $ "EVs: " <> ansiExpr evs

    pure $ True `shouldBe` False
