{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Plutus.ToPlutusData (
  roundTripPlutusDataSpec,
  sameAsToBuiltinDataSpec,
) where

import Cardano.Ledger.Plutus.ToPlutusData
import qualified PlutusLedgerApi.V4 as PV4
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Arbitrary ()
import Type.Reflection (Typeable, typeRep)

-- ==================================================================================

-- | Test the roundtrip property via 'fromPlutusData' and 'toPlutusData' for any type
--   meeting all the constraints below.
roundTripPlutusDataSpec ::
  forall x. (HasCallStack, Typeable x, Show x, Eq x, Arbitrary x, ToPlutusData x) => Spec
roundTripPlutusDataSpec =
  prop (show (typeRep @x)) $ \y ->
    fromPlutusData (toPlutusData y) `shouldBe` Just (y :: x)

sameAsToBuiltinDataSpec ::
  forall x y.
  (HasCallStack, Typeable x, Show x, Arbitrary x, ToPlutusData x, PV4.ToData y) =>
  (x -> y) ->
  Spec
sameAsToBuiltinDataSpec toPlutusType =
  prop (show (typeRep @x)) $ \(x :: x) ->
    PV4.dataToBuiltinData (toPlutusData x) `shouldBe` PV4.toBuiltinData (toPlutusType x)
