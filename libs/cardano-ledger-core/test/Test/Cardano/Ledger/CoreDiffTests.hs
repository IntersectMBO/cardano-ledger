{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.CoreDiffTests (diffTests) where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.DPState (
  DPState (..),
  DState (..),
  InstantaneousRewards (..),
  PState (..),
 )
import Cardano.Ledger.UMapCompact (
  RDPair,
  UMap,
 )
import Test.Cardano.Data (
  propExtend,
  propZero,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary (
  genCoin,
  genDiffCoin,
  genDiffDPState,
  genDiffDState,
  genDiffInstantaneousRewards,
  genDiffPState,
  genDiffRDPair,
  genDiffUMap,
 )

-- ====================================================

diffTests :: Spec
diffTests = describe "ILC Diff tests" $ do
  describe "Diff Coin" $ do
    propZero genCoin
    propExtend genCoin genDiffCoin
  describe "Diff RDPair" $ do
    propZero (arbitrary @RDPair)
    propExtend arbitrary genDiffRDPair
  describe "Diff UMap" $ do
    propZero (arbitrary @(UMap StandardCrypto))
    propExtend (arbitrary @(UMap StandardCrypto)) genDiffUMap
  describe "Diff InstantaneousRewards" $ do
    propZero (arbitrary @(InstantaneousRewards StandardCrypto))
    propExtend (arbitrary @(InstantaneousRewards StandardCrypto)) genDiffInstantaneousRewards
  describe "Diff PState" $ do
    propZero (arbitrary @(PState StandardCrypto))
    propExtend (arbitrary @(PState StandardCrypto)) genDiffPState
  describe "Diff DState" $ do
    propZero (arbitrary @(DState StandardCrypto))
    propExtend (arbitrary @(DState StandardCrypto)) genDiffDState
  describe "Diff DPState" $ do
    propZero (arbitrary @(DPState StandardCrypto))
    propExtend (arbitrary @(DPState StandardCrypto)) genDiffDPState

-- To run theses tests in ghci, uncomment and type 'main'
-- main :: IO ()
-- main = hspec $ diffTests
