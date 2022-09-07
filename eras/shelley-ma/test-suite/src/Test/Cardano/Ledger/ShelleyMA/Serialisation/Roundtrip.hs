{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Binary (Annotator (..), FromCBOR, ToCBOR)
import Cardano.Ledger.Allegra (Allegra)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Mary (Mary)
import Cardano.Ledger.Shelley (Shelley)
import Cardano.Ledger.Shelley.API (ApplyTx, ApplyTxError)
import Control.State.Transition.Extended (PredicateFailure)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Roundtrip
  ( roundTrip,
    roundTripAnn,
  )
import Data.Typeable (typeRep)
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
propertyAnn x = handleResult x $ roundTripAnn x

handleResult ::
  (Eq a1, Show a1, Show a2) =>
  a1 ->
  Either a2 (BSL.ByteString, a1) ->
  Property
handleResult x (Right (remaining, y))
  | BSL.null remaining = x === y
  | otherwise =
      counterexample
        ("Unconsumed trailing bytes:\n" <> BSL.unpack remaining)
        False
handleResult _ (Left stuff) =
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
property x = handleResult x $ roundTrip x

allprops ::
  forall e.
  ( ApplyTx e,
    Arbitrary (Core.TxBody e),
    Arbitrary (Core.AuxiliaryData e),
    Arbitrary (Core.Value e),
    Arbitrary (Core.Script e),
    Arbitrary (ApplyTxError e),
    ToCBOR (PredicateFailure (Core.EraRule "DELEGS" e)),
    FromCBOR (PredicateFailure (Core.EraRule "DELEGS" e)),
    ToCBOR (PredicateFailure (Core.EraRule "UTXOW" e)),
    FromCBOR (PredicateFailure (Core.EraRule "UTXOW" e))
  ) =>
  TestTree
allprops =
  testGroup
    (show $ typeRep (Proxy @e))
    [ testProperty "TxBody" $ propertyAnn @(Core.TxBody e),
      testProperty "Metadata" $ propertyAnn @(Core.AuxiliaryData e),
      testProperty "Value" $ property @(Core.Value e),
      testProperty "Script" $ propertyAnn @(Core.Script e),
      testProperty "ApplyTxError" $ property @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ allprops @Shelley,
      allprops @Allegra,
      allprops @Mary
    ]
