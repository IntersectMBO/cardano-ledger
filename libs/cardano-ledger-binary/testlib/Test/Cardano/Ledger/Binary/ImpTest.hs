{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Binary.ImpTest (
  RoundTripEnv (..),
  RoundTripDecoder (..),
  TwiddleEvidence (..),
  roundTripArbitrary,
  tripPlainFull,
  tripPlainFullAnn,
  tripPlainBasic,
) where

import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (..),
  Decoder,
  DecoderError,
  EncCBOR (..),
  Encoding,
  ToCBOR (..),
  Version,
  decodeFullAnnotator,
  decodeFullDecoder,
  encodeTerm,
  fromPlainEncoding,
  toPlainEncoding,
 )
import Codec.CBOR.Pretty (prettyHexEnc)
import Codec.CBOR.Write (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Data.TreeDiff (ToExpr (..), ansiWlExpr)
import Data.Typeable (Typeable, typeRep)
import Formatting.Buildable (Buildable (..))
import Prettyprinter (Doc, Pretty (..), hardline, indent, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..))
import Test.Hspec (Spec, expectationFailure)
import Test.Hspec.QuickCheck (prop)
import Test.ImpSpec (ansiDocToString)
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck.GenT (MonadGen (..))
import Text.Pretty.Simple (pShow)

data RoundTripDecoder a
  = DecodePlain (forall s. Decoder s a)
  | DecodeAnnotator (forall s. Decoder s (Annotator a))

data TwiddleEvidence a
  = Twiddle a => WithTwiddle
  | WithoutTwiddle

data ToExprEvidence a
  = ToExpr a => WithToExpr
  | WithoutToExpr

data RoundTripEnv a = RoundTripEnv
  { rteEncVersion :: Version
  , rteDecVersion :: Version
  , rteTwiddle :: TwiddleEvidence a
  , rteToExpr :: ToExprEvidence a
  , rteEncoder :: a -> Encoding
  , rteDecoder :: RoundTripDecoder a
  }

showType :: Typeable a => Proxy a -> String
showType = show . typeRep

roundTripArbitrary ::
  forall a.
  ( Arbitrary a
  , Eq a
  , Typeable a
  , Show a
  ) =>
  RoundTripEnv a ->
  Spec
roundTripArbitrary env@RoundTripEnv {..} = prop propDescription $ do
  val <- arbitrary
  encoding <- tripToCBOR env val
  let
    prettyVal :: a -> Doc AnsiStyle
    prettyVal x = case rteToExpr of
      WithToExpr -> ansiWlExpr $ toExpr x
      WithoutToExpr -> pretty $ pShow x
    plainEncoding = toPlainEncoding rteEncVersion encoding
    extraInfo =
      [ "Initial value:"
      , indent 2 $ prettyVal val
      , hardline
      , "Encoded bytes:"
      , indent 2 . pretty $ prettyHexEnc plainEncoding
      , hardline
      ]
  pure $ case tripFromCBOR env $ toLazyByteString plainEncoding of
    Right x
      | x == val -> pure ()
      | otherwise ->
          expectationFailure
            . ansiDocToString
            . vsep
            $ extraInfo
              <> [ "Final value:"
                 , prettyVal x
                 , hardline
                 , "Decoding succeeded, but the values are different"
                 ]
    Left e ->
      expectationFailure
        . ansiDocToString
        . vsep
        $ extraInfo
          <> [ pretty . toLazyText $ build e
             ]
  where
    roundTripEnvNotes =
      mconcat
        [ ["annotated" | DecodeAnnotator _ <- pure rteDecoder]
        , ["twiddled" | WithTwiddle <- pure rteTwiddle]
        ]
    roundTripEnvSummary = "(" <> intercalate "," roundTripEnvNotes <> ")"
    propDescription =
      "Round-tripping `" <> showType (Proxy @a) <> "` " <> roundTripEnvSummary

tripToCBOR :: RoundTripEnv a -> a -> Gen Encoding
tripToCBOR RoundTripEnv {..} val = do
  let
    encoded = rteEncoder val
  case rteTwiddle of
    WithTwiddle -> do
      twiddledTerm <- liftGen $ twiddle rteEncVersion val
      pure $ encodeTerm twiddledTerm
    WithoutTwiddle -> pure encoded

tripFromCBOR ::
  forall a.
  Typeable a => RoundTripEnv a -> LBS.ByteString -> Either DecoderError a
tripFromCBOR RoundTripEnv {..} bytes = do
  let typeName = T.pack . showType $ Proxy @a
  case rteDecoder of
    DecodeAnnotator dec -> decodeFullAnnotator rteDecVersion typeName dec bytes
    DecodePlain dec -> decodeFullDecoder rteDecVersion typeName dec bytes

tripPlainFull :: forall a. (Twiddle a, EncCBOR a, DecCBOR a, ToExpr a) => Version -> RoundTripEnv a
tripPlainFull version =
  RoundTripEnv
    { rteEncVersion = version
    , rteDecVersion = version
    , rteTwiddle = WithTwiddle
    , rteToExpr = WithToExpr
    , rteEncoder = encCBOR
    , rteDecoder = DecodePlain decCBOR
    }

tripPlainFullAnn ::
  (ToCBOR a, DecCBOR (Annotator a), Twiddle a, ToExpr a) => Version -> RoundTripEnv a
tripPlainFullAnn version =
  RoundTripEnv
    { rteEncVersion = version
    , rteDecVersion = version
    , rteTwiddle = WithTwiddle
    , rteToExpr = WithToExpr
    , rteEncoder = fromPlainEncoding . toCBOR
    , rteDecoder = DecodeAnnotator decCBOR
    }

tripPlainBasic :: (EncCBOR a, DecCBOR a) => Version -> RoundTripEnv a
tripPlainBasic version =
  RoundTripEnv
    { rteEncVersion = version
    , rteDecVersion = version
    , rteTwiddle = WithoutTwiddle
    , rteToExpr = WithoutToExpr
    , rteEncoder = encCBOR
    , rteDecoder = DecodePlain decCBOR
    }
