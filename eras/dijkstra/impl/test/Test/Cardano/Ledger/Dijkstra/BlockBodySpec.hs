{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Dijkstra.BlockBodySpec (spec) where

import Cardano.Ledger.BaseTypes (LeiosCert (..), PerasCert (..))
import Cardano.Ledger.Binary
import Data.Maybe.Strict (StrictMaybe (..))
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "Dijkstra block body cert slots" $ do
  -- Regression: the placeholder 'LeiosCert' / 'PerasCert' types
  -- used to encode as 'encCBOR ()' = CBOR null (0xf6). When such a
  -- cert is wrapped in 'StrictMaybe' and serialised via
  -- 'encodeNullStrictMaybe' (which emits CBOR null for 'SNothing'),
  -- the wire 0xf6 is ambiguous between 'SJust' and 'SNothing' —
  -- and 'decodeNullStrictMaybe' interprets it as 'SNothing'. That
  -- silently dropped Leios certs across the wire on the Dijkstra
  -- Leios cert block path. Lock in a non-null encoding for both
  -- placeholders so 'SJust cert' survives the roundtrip.
  it "SJust LeiosCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip (SJust LeiosCert) `shouldBe` Right (SJust LeiosCert)
  it "SNothing @LeiosCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip (SNothing :: StrictMaybe LeiosCert)
      `shouldBe` Right (SNothing :: StrictMaybe LeiosCert)
  it "SJust PerasCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip (SJust PerasCert) `shouldBe` Right (SJust PerasCert)
  it "SNothing @PerasCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip (SNothing :: StrictMaybe PerasCert)
      `shouldBe` Right (SNothing :: StrictMaybe PerasCert)

-- | Encode/decode 'StrictMaybe a' through the null-tagged pair the
-- Dijkstra block body uses for its 'LeiosCert' / 'PerasCert' slots.
strictMaybeCborRoundtrip ::
  forall a.
  (EncCBOR a, DecCBOR a) =>
  StrictMaybe a ->
  Either DecoderError (StrictMaybe a)
strictMaybeCborRoundtrip x =
  decodeFullDecoder
    shelleyProtVer
    "StrictMaybe roundtrip"
    (decodeNullStrictMaybe decCBOR)
    (serialize shelleyProtVer (encodeNullStrictMaybe encCBOR x))
