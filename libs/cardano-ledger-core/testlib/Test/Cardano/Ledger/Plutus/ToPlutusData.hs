{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Plutus.ToPlutusData (
  roundTripPlutusDataSpec,
) where

import Cardano.Ledger.Plutus.ToPlutusData
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
