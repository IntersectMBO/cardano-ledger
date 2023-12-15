{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.TreeDiff (
  ToExpr (..),
  CBORBytes (..),
  HexBytes (..),
  showExpr,
  diffExpr,
  diffExprCompact,
  diffExprNoColor,
  hexByteStringExpr,
  showHexBytesGrouped,
  expectExprEqual,
  expectExprEqualWithMessage,
  assertExprEqualWithMessage,
  Expr (App, Rec, Lst),
  defaultExprViaShow,
  trimExprViaShow,
)
where

import qualified Cardano.Binary as Plain
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Class ()
import Cardano.Ledger.Binary
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import Control.Monad (unless)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.IP (IPv4, IPv6)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.TreeDiff
import Test.Cardano.Slotting.TreeDiff ()
import Test.Hspec (Expectation, HasCallStack, expectationFailure)
import Test.Tasty.HUnit (Assertion, assertFailure)

-- =====================================================
-- Cardano functions that deal with TreeDiff and ToExpr

trimExprViaShow :: Show a => Int -> a -> Expr
trimExprViaShow _n x = defaultExprViaShow x -- App (take n (drop 1 (show x)) ++ "..") []

-- ===========================================================
-- Orphan instances from external imports

instance ToExpr IPv4

instance ToExpr IPv6

instance ToExpr (Hash.Hash c index) where
  toExpr = trimExprViaShow 10

instance DSIGN.DSIGNAlgorithm c => ToExpr (DSIGN.SignedDSIGN c index) where
  toExpr = trimExprViaShow 10

instance ToExpr a => ToExpr (StrictSeq a) where
  toExpr x = App "StrictSeqFromList" [listToExpr (toList x)]

instance ToExpr a => ToExpr (StrictMaybe a)

instance ToExpr Version where
  toExpr v = App "Version" [toExpr (getVersion64 v)]

instance ToExpr a => ToExpr (Sized a)

--------------------------------------------------------------------------------
--  Diffing and pretty showing CBOR
--------------------------------------------------------------------------------

showExpr :: ToExpr a => a -> String
showExpr = show . ansiWlExpr . toExpr

diffExpr :: ToExpr a => a -> a -> String
diffExpr x y = show (ansiWlEditExpr (ediff x y))

diffExprNoColor :: ToExpr a => a -> a -> String
diffExprNoColor x y = show (prettyEditExpr (ediff x y))

diffExprCompact :: ToExpr a => a -> a -> String
diffExprCompact x y = show (ansiWlEditExprCompact (ediff x y))

-- | Wraps regular ByteString, but shows and diffs it as hex
newtype HexBytes = HexBytes {unHexBytes :: BS.ByteString}
  deriving (Eq)

instance Show HexBytes where
  show = showExpr

instance ToExpr HexBytes where
  toExpr = App "HexBytes" . hexByteStringExpr . unHexBytes

newtype CBORBytes = CBORBytes {unCBORBytes :: BS.ByteString}
  deriving (Eq)

instance Show CBORBytes where
  show = showExpr

instance ToExpr CBORBytes where
  toExpr (CBORBytes bytes) =
    case CBOR.deserialiseFromBytes CBOR.decodeTerm (BSL.fromStrict bytes) of
      Left err ->
        App
          "CBORBytesError"
          [ toExpr @String "Error decoding CBOR, showing as Hex:"
          , toExpr (HexBytes bytes)
          , toExpr $ show err
          ]
      Right (leftOver, term)
        | BSL.null leftOver -> App "CBORBytes" [toExpr term]
        | otherwise ->
            case Plain.decodeFullDecoder "Term" CBOR.decodeTerm leftOver of
              Right leftOverTerm ->
                App
                  "CBORBytesError"
                  [ toExpr @String "Error decoding CBOR fully:"
                  , toExpr term
                  , toExpr @String "Leftover:"
                  , toExpr (leftOverTerm :: Term)
                  ]
              Left err ->
                App
                  "CBORBytesError"
                  [ toExpr @String "Error decoding CBOR fully:"
                  , toExpr term
                  , toExpr @String "Leftover as Hex, due to inabilty to decode as Term:"
                  , toExpr $ HexBytes $ BSL.toStrict leftOver
                  , toExpr $ showDecoderError err
                  ]

instance ToExpr Term where
  toExpr =
    \case
      TInt i -> App "TInt" [toExpr i]
      TInteger i -> App "TInteger" [toExpr i]
      TBytes bs -> App "TBytes" $ hexByteStringExpr bs
      TBytesI bs -> App "TBytesI" $ hexByteStringExpr $ BSL.toStrict bs
      TString s -> App "TString" [toExpr s]
      TStringI s -> App "TStringI" [toExpr s]
      TList xs -> App "TList" [Lst (map toExpr xs)]
      TListI xs -> App "TListI" [Lst (map toExpr xs)]
      TMap xs -> App "TMap" [Lst (map (toExpr . bimap toExpr toExpr) xs)]
      TMapI xs -> App "TMapI" [Lst (map (toExpr . bimap toExpr toExpr) xs)]
      TTagged 24 (TBytes x) -> App "CBOR-in-CBOR" [toExpr (CBORBytes x)]
      TTagged t x -> App "TTagged" [toExpr t, toExpr x]
      TBool x -> App "TBool" [toExpr x]
      TNull -> App "TNull" []
      TSimple x -> App "TSimple" [toExpr x]
      THalf x -> App "THalf" [toExpr x]
      TFloat x -> App "TFloat" [toExpr x]
      TDouble x -> App "TDouble" [toExpr x]

hexByteStringExpr :: BS.ByteString -> [Expr]
hexByteStringExpr bs =
  [ toExpr (BS.length bs)
  , Lst (map toExpr $ showHexBytesGrouped bs)
  ]

-- | Show a ByteString as hex groups of 8bytes each. This is a slightly more
-- useful form for debugging, rather than bunch of escaped characters.
showHexBytesGrouped :: BS.ByteString -> [String]
showHexBytesGrouped bs
  | BS.null bs = []
  | otherwise =
      ("0x" <> BS8.unpack (BS.take 128 bs16))
        : [ "  " <> BS8.unpack (BS.take 128 $ BS.drop i bs16)
          | i <- [128, 256 .. BS.length bs16 - 1]
          ]
  where
    bs16 = Base16.encode bs

-- | Check that two values are equal and if they are not raise an exception with the
-- `ToExpr` diff
expectExprEqual :: (Eq a, ToExpr a) => a -> a -> Expectation
expectExprEqual = expectExprEqualWithMessage "Expected two values to be equal:"

-- | Use this with HSpec, but with Tasty use 'assertExprEqualWithMessage' below
expectExprEqualWithMessage :: (ToExpr a, Eq a, HasCallStack) => [Char] -> a -> a -> Expectation
expectExprEqualWithMessage message expected actual =
  unless (actual == expected) (expectationFailure msg)
  where
    msg = (if null message then "" else message ++ "\n") ++ diffExpr expected actual

-- | Use this with Tasty, but with HSpec use 'expectExprEqualWithMessage' above
assertExprEqualWithMessage :: (ToExpr a, Eq a, HasCallStack) => [Char] -> a -> a -> Assertion
assertExprEqualWithMessage message expected actual =
  unless (actual == expected) (assertFailure msg)
  where
    msg = (if null message then "" else message ++ "\n") ++ diffExpr expected actual
