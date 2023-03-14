{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.Success (spec) where

import Cardano.Ledger.Binary
import Control.Monad (forM_)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Test.Cardano.Ledger.Binary.RoundTrip (Trip (..), embedTripExpectation)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Generate a list with unique elements
genIntSet :: Gen (Set.Set Int)
genIntSet = Set.fromList <$> listOf chooseAny

-- | Generate a CBOR encoded list with no duplicates, with and with the set tag
genUniqueListEncoding :: Gen (Set.Set Int, Encoding)
genUniqueListEncoding = do
  xsSet <- genIntSet
  let xs = Set.toList xsSet
      definite = encodeListLen (fromIntegral $ Prelude.length xs) <> foldMap encCBOR xs
      indefinite = encodeListLenIndef <> foldMap encCBOR xs <> encodeBreak
  (,) xsSet
    <$> elements
      [ definite
      , encodeTag 258 <> definite
      , indefinite
      , encodeTag 258 <> indefinite
      ]

-- | Starting in version 9, check set decoding with and without tag 258
prop_setWithNoDuplicatesAndTag :: Property
prop_setWithNoDuplicatesAndTag =
  forAllBlind genUniqueListEncoding $
    \(s, setEncoder) ->
      let trip = Trip id (decCBOR @(Set.Set Int)) (dropCBOR (Proxy @(Set.Set Int)))
       in property $
            forM_ [(natVersion @9) .. maxBound] $
              \v -> embedTripExpectation v v trip (\s' _ -> (s' `shouldBe` s)) setEncoder

spec :: Spec
spec = do
  describe "Successes" $ do
    prop "encode Set, v9" prop_setWithNoDuplicatesAndTag
