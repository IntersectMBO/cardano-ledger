{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Plutus.ToPlutusData (
  plutusDataRoundtripSpec,
  roundtripPlutusDataSpec,
) where

import Cardano.Ledger.BaseTypes (
  EpochInterval (..),
  NonNegativeInterval,
  ProtVer (..),
  UnitInterval,
 )
import Cardano.Ledger.Binary.Version (Version)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Plutus.CostModels (CostModels)
import Cardano.Ledger.Plutus.ExUnits (ExUnits, Prices)
import Cardano.Ledger.Plutus.ToPlutusData
import Data.Map.Strict (Map)
import Data.Word
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Type.Reflection (Typeable, typeRep)

-- ==================================================================================

-- | Test the roundtrip property via 'fromPlutusData' and 'toPlutusData' for any type
--   meeting all the constraints below.
roundtripPlutusDataSpec :: forall x. (HasCallStack, Typeable x, Show x, Eq x, Arbitrary x, ToPlutusData x) => Spec
roundtripPlutusDataSpec =
  prop (show (typeRep @x)) $ \y ->
    fromPlutusData (toPlutusData y) `shouldBe` Just (y :: x)

plutusDataRoundtripSpec :: Spec
plutusDataRoundtripSpec = do
  describe "roundtrip ToPlutusData" $ do
    roundtripPlutusDataSpec @Version
    roundtripPlutusDataSpec @Word
    roundtripPlutusDataSpec @Word8
    roundtripPlutusDataSpec @Word16
    roundtripPlutusDataSpec @Word32
    roundtripPlutusDataSpec @[Word]
    roundtripPlutusDataSpec @[Word8]
    roundtripPlutusDataSpec @(Map Word Version)
    roundtripPlutusDataSpec @Coin
    roundtripPlutusDataSpec @ExUnits
    roundtripPlutusDataSpec @Prices
    roundtripPlutusDataSpec @Natural
    roundtripPlutusDataSpec @UnitInterval
    roundtripPlutusDataSpec @EpochInterval
    roundtripPlutusDataSpec @NonNegativeInterval
    roundtripPlutusDataSpec @ProtVer
    roundtripPlutusDataSpec @CostModels
    roundtripPlutusDataSpec @Integer
