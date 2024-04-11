{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conformance.ConformanceSpec (spec) where

import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Data.List (isInfixOf)
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Conformance (SpecTranslate (..), runSpecTransM)
import Test.Cardano.Ledger.Conformance.Spec.Conway ()
import Test.Cardano.Ledger.Conformance.Utils (agdaHashToBytes)

spec :: Spec
spec =
  describe "Translation" $ do
    prop "Hashes are displayed in the same way in the implementation and in the spec" $ do
      someHash <- arbitrary @(KeyHash 'Staking StandardCrypto)
      let
        specRes =
          case runSpecTransM () (toTestRep someHash) of
            Left e -> error $ "Failed to translate hash: " <> show e
            Right x -> agdaHashToBytes x
      pure $ show specRes `isInfixOf` showExpr someHash
