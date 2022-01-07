{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Binary (Annotator (..), FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.API (ApplyTx, ApplyTxError)
import Cardano.Ledger.Shelley.Constraints
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Roundtrip
  ( roundTrip,
    roundTripAnn,
  )
import Data.Typeable (typeRep)
import Test.Cardano.Ledger.EraBuffet
import Test.Cardano.Ledger.Shelley.Generator.Metadata ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary, Property, counterexample, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

propertyAnn ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR (Annotator t)
  ) =>
  t ->
  Property
propertyAnn x = case roundTripAnn x of
  Right (remaining, y) | BSL.null remaining -> x === y
  Right (remaining, _) ->
    counterexample
      ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
      False
  Left stuff ->
    counterexample
      ("Failed to decode: " <> show stuff)
      False

property ::
  forall t.
  ( Eq t,
    Show t,
    ToCBOR t,
    FromCBOR t
  ) =>
  t ->
  Property
property x =
  case roundTrip x of
    Right (remaining, y) | BSL.null remaining -> x === y
    Right (remaining, _) ->
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
    Left stuff ->
      counterexample
        ("Failed to decode: " <> show stuff)
        False

allprops ::
  forall e.
  ( UsesValue e,
    UsesScript e,
    UsesTxBody e,
    UsesAuxiliary e,
    ApplyTx e,
    Arbitrary (Core.TxBody e),
    Arbitrary (Core.AuxiliaryData e),
    Arbitrary (Core.Value e),
    Arbitrary (Core.Script e),
    Arbitrary (ApplyTxError e)
  ) =>
  TestTree
allprops =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "TxBody" $ propertyAnn @(Core.TxBody e),
      testProperty "Metadata" $ propertyAnn @(Core.AuxiliaryData e),
      testProperty "Value" $ property @(Core.Value e),
      testProperty "Script" $ propertyAnn @(Core.Script e),
      testProperty "ApplyTxError" $ propertyAnn @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ allprops @(ShelleyEra StandardCrypto),
      allprops @(AllegraEra StandardCrypto),
      allprops @(MaryEra StandardCrypto)
    ]
