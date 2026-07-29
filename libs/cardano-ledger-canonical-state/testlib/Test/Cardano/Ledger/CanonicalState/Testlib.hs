{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Test suite utilities for the implementor
module Test.Cardano.Ledger.CanonicalState.Testlib (
  testAllNS,

  -- * Hspec helpers
  testNS,
  validateType,

  -- * properties
  propNamespaceEntryConformsToSpec,
  propNamespaceEntryIsCanonical,
  propTypeIsCanonical,
  propNamespaceEntryRoundTrip,
  propTypeConformsToSpec,

  -- * Debug tools
  debugValidateType,
  debugEncodeType,
) where

import Cardano.Ledger.CanonicalState.CDDL.Validate (validateBytesAgainst)
import Cardano.SCLS.CBOR.Canonical (getRawDecoder, getRawEncoding)
import Cardano.SCLS.CBOR.Canonical.Encoder
import Cardano.SCLS.NamespaceCodec
import Cardano.SCLS.Versioned
import Codec.CBOR.Cuddle.CBOR.Validator.Trace (Evidenced, ValidationTrace)
import qualified Codec.CBOR.Cuddle.CBOR.Validator.Trace as VT
import Codec.CBOR.FlatTerm (fromFlatTerm, toFlatTerm)
import Codec.CBOR.Term (decodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import Data.Either (isRight)
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import GHC.TypeLits
import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)
import Test.Hspec.QuickCheck
import Test.QuickCheck

type ConstrNS a =
  (KnownNamespace a, Arbitrary (NamespaceEntry a), Eq (NamespaceEntry a), Show (NamespaceEntry a))

-- | Test all supported NS for conformance with SCLS.
testAllNS ::
  ( ConstrNS "blocks/v0"
  , ConstrNS "utxo/v0"
  , ConstrNS "entities/accounts/v0"
  , ConstrNS "entities/committee/v0"
  , ConstrNS "entities/dreps/v0"
  , ConstrNS "entities/stake_pools/v0"
  , ConstrNS "entities/stake_pools/vrf_key_hashes/v0"
  , ConstrNS "gov/committee/v0"
  , ConstrNS "gov/constitution/v0"
  , ConstrNS "gov/pparams/v0"
  , ConstrNS "gov/proposals/v0"
  , ConstrNS "gov/proposals/roots/v0"
  ) =>
  Spec
testAllNS = describe "scls/conformance" $ do
  testNS @"blocks/v0"
  testNS @"utxo/v0"
  testNS @"entities/accounts/v0"
  testNS @"entities/committee/v0"
  testNS @"entities/dreps/v0"
  testNS @"entities/stake_pools/v0"
  testNS @"entities/stake_pools/vrf_key_hashes/v0"
  testNS @"gov/committee/v0"
  testNS @"gov/constitution/v0"
  testNS @"gov/pparams/v0"
  testNS @"gov/proposals/v0"
  testNS @"gov/proposals/roots/v0"

-- | Validate concrete type against its definition in CDDL
validateType ::
  forall ns a.
  (KnownSymbol ns, ToCanonicalCBOR ns a, Arbitrary a, Show a, Typeable a) => T.Text -> Spec
validateType t = prop ("validate type<" ++ n ++ ">") (propTypeConformsToSpec @ns @a t)
  where
    n = show (typeRep (Proxy @a))

testNS ::
  forall ns.
  ( KnownSymbol ns
  , KnownNamespace ns
  , Arbitrary (NamespaceEntry ns)
  , Eq (NamespaceEntry ns)
  , Show (NamespaceEntry ns)
  ) =>
  Spec
testNS =
  describe nsName $ do
    prop "conforms to spec" $
      propNamespaceEntryConformsToSpec @ns
    prop "canonical with regards to its definition" $
      propNamespaceEntryRoundTrip @ns
    prop "is canonical" $
      propNamespaceEntryIsCanonical @ns
  where
    nsName = symbolVal (Proxy @ns)

-- | Each value from the known namespace conforms to its spec
propNamespaceEntryConformsToSpec ::
  forall ns.
  (KnownSymbol ns, KnownNamespace ns) => NamespaceEntry ns -> Bool
propNamespaceEntryConformsToSpec = \a ->
  case validateBytesAgainst (toStrictByteString (getRawEncoding $ encodeEntry @ns a)) nsName "record_entry" of
    Just res -> VT.isValid res
    _ -> False
  where
    nsName = T.pack (symbolVal (Proxy @ns))

propTypeConformsToSpec :: forall ns a. (KnownSymbol ns, ToCanonicalCBOR ns a) => T.Text -> a -> Bool
propTypeConformsToSpec t = \a ->
  case validateBytesAgainst (toStrictByteString $ getRawEncoding (toCanonicalCBOR (Proxy @ns) a)) nsName t of
    Just res -> VT.isValid res
    _ -> False
  where
    nsName = T.pack (symbolVal (Proxy @ns))

propNamespaceEntryIsCanonical ::
  forall ns.
  KnownNamespace ns => NamespaceEntry ns -> IO ()
propNamespaceEntryIsCanonical a =
  let encodedData = toFlatTerm (getRawEncoding $ encodeEntry @ns a)
   in case fromFlatTerm decodeTerm encodedData of
        Right decodedAsTerm -> annotate "(b, t) = decode @Term (encode x)" $ do
          let encodedTerm = toFlatTerm (getRawEncoding $ toCanonicalCBOR (Proxy @ns) decodedAsTerm)
          encodedTerm `shouldBe` encodedData
        r -> r `shouldSatisfy` isRight

-- | Namespace entry are not contradictory and can roundtrip: `decode.encode.decode.encode = decode.encode`
--
-- We do not require `decode.encode=id` because we do not require input type to be in canonical form.
--
-- I.e. if we have types:
--
-- ```
-- data V a = NoHash a | WithHash a (Maybe Hash)
-- ```
--
-- And it's ok to decode `WithHash a Nothing` to `NoHash a`, `decode.encode=id` property will fail, because
-- decoding will put the value in it's canonical form.
propNamespaceEntryRoundTrip ::
  forall ns.
  (KnownNamespace ns, Eq (NamespaceEntry ns), Show (NamespaceEntry ns)) =>
  NamespaceEntry ns -> IO ()
propNamespaceEntryRoundTrip a = do
  case fromFlatTerm (getRawDecoder $ decodeEntry @ns) (toFlatTerm (getRawEncoding $ encodeEntry @ns a)) of
    Right (Versioned a') -> annotate "(b, a') = decode (encode a)" $ do
      a' `shouldBe` a
      case fromFlatTerm (getRawDecoder $ decodeEntry @ns) (toFlatTerm (getRawEncoding $ encodeEntry @ns a')) of
        Right (Versioned a'') -> annotate "(b', a'') = decode (encode a')" $ do
          a'' `shouldBe` a'
        r -> r `shouldSatisfy` isRight
    r -> r `shouldSatisfy` isRight

debugValidateType ::
  forall ns a.
  (KnownSymbol ns, ToCanonicalCBOR ns a) => T.Text -> a -> Maybe (Evidenced ValidationTrace)
debugValidateType t a =
  validateBytesAgainst (toStrictByteString $ getRawEncoding (toCanonicalCBOR (Proxy @ns) a)) nsName t
  where
    nsName = T.pack (symbolVal (Proxy @ns))

-- | Serialize value to CBOR (for usage in debug tools)
debugEncodeType :: forall ns a. ToCanonicalCBOR ns a => a -> B.ByteString
debugEncodeType a = Base16.encode $ toStrictByteString $ getRawEncoding (toCanonicalCBOR (Proxy @ns) a)

propTypeIsCanonical :: forall ns a. ToCanonicalCBOR ns a => a -> IO ()
propTypeIsCanonical a =
  let encodedData = toFlatTerm (getRawEncoding $ toCanonicalCBOR (Proxy @ns) a)
   in case fromFlatTerm decodeTerm encodedData of
        Right decodedAsTerm -> annotate "(b, t) = decode @Term (encode x)" $ do
          let encodedTerm = toFlatTerm (getRawEncoding $ toCanonicalCBOR (Proxy @ns) decodedAsTerm)
          encodedTerm `shouldBe` encodedData
        r -> r `shouldSatisfy` isRight
