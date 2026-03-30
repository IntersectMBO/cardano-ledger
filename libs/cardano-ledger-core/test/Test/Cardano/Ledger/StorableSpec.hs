{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.StorableSpec (spec) where

import Cardano.Ledger.Credential
import Cardano.Ledger.Hashes
import Cardano.Ledger.State
import Foreign.Storable
import Test.Cardano.Base.Properties (expectStorable)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()

spec :: Spec
spec = describe "Storable" $ do
  prop "KeyHash" $ \(hash :: KeyHash Witness, offset, slack) -> do
    sizeOf hash `shouldBe` 28
    alignment hash `shouldBe` 32
    expectStorable hash offset slack
  prop "ScriptHash" $ \(hash :: ScriptHash, offset, slack) -> do
    sizeOf hash `shouldBe` 28
    alignment hash `shouldBe` 32
    expectStorable hash offset slack
  prop "Credential" $ \(cred :: Credential Witness, offset, slack) -> do
    sizeOf cred `shouldBe` 29
    alignment cred `shouldBe` 32
    expectStorable cred offset slack
  prop "StakeWithDelegation" $ \(swd :: StakeWithDelegation, offset, slack) -> do
    sizeOf swd `shouldBe` 36
    alignment swd `shouldBe` 8
    expectStorable swd offset slack
