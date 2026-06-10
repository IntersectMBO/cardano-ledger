{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Dijkstra.BlockBodySpec (spec) where

import Cardano.Crypto.DSIGN (genKeyDSIGN, signDSIGN)
import Cardano.Crypto.DSIGN.BLS12381 (BLS12381SignContext (..))
import Cardano.Crypto.Leios (
  EbHash (..),
  LeiosCert (..),
  LeiosDSIGN,
  LeiosSignature,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Ledger.BaseTypes (PerasCert (..))
import Cardano.Ledger.Binary
import Cardano.Ledger.Dijkstra.BlockBody (
  decodeLeiosCert,
  encodeLeiosCert,
 )
import qualified Data.ByteString as BS
import Data.Maybe.Strict (StrictMaybe (..))
import Test.Cardano.Ledger.Common

-- | A real (deterministic) BLS12-381 MinSig signature, since the BLS
-- deserialiser enforces subgroup membership and won't accept arbitrary
-- 48-byte input.
sampleSignature :: LeiosSignature
sampleSignature =
  signDSIGN
    (BLS12381SignContext Nothing Nothing)
    ("leios-cert-sample" :: BS.ByteString)
    (genKeyDSIGN @LeiosDSIGN (mkSeedFromBytes (BS.replicate 32 0x01)))

sampleLeiosCert :: LeiosCert
sampleLeiosCert =
  LeiosCert
    { slotNo = 42
    , endorserBlockHash = MkEbHash (BS.replicate 32 0x7a)
    , signers = BS.replicate 8 0xff
    , aggregatedSignature = sampleSignature
    }

spec :: Spec
spec = describe "Dijkstra block body cert slots" $ do
  -- Regression: the placeholder 'PerasCert' type used to encode as
  -- 'encCBOR ()' = CBOR null (0xf6). When such a cert is wrapped in
  -- 'StrictMaybe' and serialised via 'encodeNullStrictMaybe' (which
  -- emits CBOR null for 'SNothing'), the wire 0xf6 is ambiguous
  -- between 'SJust' and 'SNothing' — and 'decodeNullStrictMaybe'
  -- interprets it as 'SNothing'. That silently dropped certs across
  -- the wire on the Dijkstra Leios cert block path. Lock in a
  -- non-null encoding so 'SJust cert' survives the roundtrip.
  it "SJust LeiosCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip encodeLeiosCert decodeLeiosCert (SJust sampleLeiosCert)
      `shouldBe` Right (SJust sampleLeiosCert)
  it "SNothing @LeiosCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip encodeLeiosCert decodeLeiosCert (SNothing :: StrictMaybe LeiosCert)
      `shouldBe` Right (SNothing :: StrictMaybe LeiosCert)
  it "SJust PerasCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip encCBOR decCBOR (SJust PerasCert)
      `shouldBe` Right (SJust PerasCert)
  it "SNothing @PerasCert roundtrips through StrictMaybe" $
    strictMaybeCborRoundtrip encCBOR decCBOR (SNothing :: StrictMaybe PerasCert)
      `shouldBe` Right (SNothing :: StrictMaybe PerasCert)

-- | Encode/decode 'StrictMaybe a' through the null-tagged pair the
-- Dijkstra block body uses for its 'LeiosCert' / 'PerasCert' slots.
strictMaybeCborRoundtrip ::
  forall a.
  (a -> Encoding) ->
  (forall s. Decoder s a) ->
  StrictMaybe a ->
  Either DecoderError (StrictMaybe a)
strictMaybeCborRoundtrip enc dec x =
  decodeFullDecoder
    shelleyProtVer
    "StrictMaybe roundtrip"
    (decodeNullStrictMaybe dec)
    (serialize shelleyProtVer (encodeNullStrictMaybe enc x))
