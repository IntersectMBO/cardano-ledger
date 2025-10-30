{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Test.Cardano.Ledger.Conway.Binary.Golden (
  expectDecoderResultOn,
  expectDecoderFailure,
  listRedeemersEnc,
  goldenListRedeemers,
) where

import Cardano.Ledger.Alonzo.Core (
  AsIx (..),
  eraProtVerLow,
  pattern SpendingPurpose,
 )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), unRedeemers)
import Cardano.Ledger.BaseTypes (Version)
import Cardano.Ledger.Binary (
  Annotator (..),
  DecCBOR (..),
  ToCBOR (..),
  decodeFullAnnotator,
  toLazyByteString,
 )
import Cardano.Ledger.Binary.Plain (DecoderError (..), Tokens (..))
import Cardano.Ledger.Plutus (Data (..))
import qualified Data.Map as Map
import Data.Typeable (Proxy (..), Typeable)
import PlutusLedgerApi.Common (Data (..))
import Test.Cardano.Ledger.Binary.Plain.Golden (Enc (..))
import Test.Cardano.Ledger.Binary.RoundTrip (embedTripAnnExpectation)
import Test.Cardano.Ledger.Common (
  Expectation,
  HasCallStack,
  Spec,
  ToExpr,
  expectationFailure,
  it,
  shouldBe,
  shouldBeExpr,
  showExpr,
 )
import Test.Cardano.Ledger.Conway.Era (ConwayEraTest)

expectDecoderFailure ::
  forall a.
  (ToExpr a, DecCBOR (Annotator a), Typeable a, HasCallStack) =>
  Version ->
  Enc ->
  DecoderError ->
  Expectation
expectDecoderFailure version enc expectedErr =
  case result of
    Left err -> err `shouldBe` expectedErr
    Right x ->
      expectationFailure $
        "Expected a failure, but decoder succeeded:\n"
          <> showExpr x
  where
    bytes = toLazyByteString $ toCBOR enc
    result = decodeFullAnnotator @a version (label $ Proxy @(Annotator a)) decCBOR bytes

expectDecoderResultOn ::
  forall a b.
  (ToExpr b, DecCBOR (Annotator a), Eq b, HasCallStack) =>
  Version -> Enc -> a -> (a -> b) -> Expectation
expectDecoderResultOn version enc expected f =
  embedTripAnnExpectation version version (\x _ -> f x `shouldBeExpr` f expected) enc

-- | A simple redeemer encoded as a list
listRedeemersEnc :: Enc
listRedeemersEnc =
  mconcat
    [ E (TkListLen 1)
    , mconcat
        [ E (TkListLen 4)
        , E (0 :: Int)
        , E (10 :: Int)
        , E (20 :: Int)
        , mconcat
            [ E (TkListLen 2)
            , E (30 :: Int)
            , E (40 :: Int)
            ]
        ]
    ]

goldenListRedeemers :: forall era. ConwayEraTest era => Spec
goldenListRedeemers =
  it "Decoding Redeemers encoded as a list succeeds" $
    expectDecoderResultOn @(Redeemers era)
      (eraProtVerLow @era)
      listRedeemersEnc
      (Redeemers $ Map.singleton (SpendingPurpose $ AsIx 10) (Data $ I 20, ExUnits 30 40))
      unRedeemers
