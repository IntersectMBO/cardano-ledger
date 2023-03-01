{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Binary.PlainSpec (spec) where

import Cardano.Ledger.Binary.Plain
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "Plain" $ do
    prop "decodeFullFromHexText . serializeAsHexText" $ \(term :: Term) ->
      case decodeFullFromHexText $ serializeAsHexText term of
        Left err -> expectationFailure $ "Unexpected hex rountrip error: " ++ show err
        Right (res :: Term) -> res `shouldBe` term
