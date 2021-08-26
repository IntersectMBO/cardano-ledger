{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Roundtrip where

import Cardano.Binary (Annotator (..), FromCBOR, ToCBOR)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.Constraints (ShelleyBased)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import Shelley.Spec.Ledger.API (ApplyTx, ApplyTxError)
import Test.Cardano.Ledger.EraBuffet
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Coders
  ( roundTrip,
    roundTripAnn,
  )
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.QuickCheck (Arbitrary, Property, counterexample, (===))
import Test.Shelley.Spec.Ledger.Generator.Metadata ()
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
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
  ( ShelleyBased e,
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
      testProperty "ApplyTxError" $ property @(ApplyTxError e)
    ]

allEraRoundtripTests :: TestTree
allEraRoundtripTests =
  testGroup
    "All Era Roundtrip Tests"
    [ allprops @(ShelleyEra StandardCrypto),
      allprops @(AllegraEra StandardCrypto),
      allprops @(MaryEra StandardCrypto)
    ]
