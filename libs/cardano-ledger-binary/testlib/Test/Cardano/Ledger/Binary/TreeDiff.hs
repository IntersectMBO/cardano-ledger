{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.TreeDiff (
  CBORBytes (..),
  HexBytes (..),
  showExpr,
  diffExpr,
  hexByteStringExpr,
  showHexBytesGrouped,
  expectExprEqual,
  expectExprEqualWithMessage,
  assertExprEqualWithMessage,
)
where

import Cardano.Ledger.Binary
import Cardano.Ledger.TreeDiff (diffExpr)
import Control.Monad (unless)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.TreeDiff
import GHC.Exts
import Test.Hspec (Expectation, HasCallStack, expectationFailure)
import Test.Tasty.HUnit (Assertion, assertFailure)

--------------------------------------------------------------------------------
--  Diffing and pretty showing CBOR
--------------------------------------------------------------------------------

showExpr :: ToExpr a => a -> String
showExpr = show . ansiWlExpr . toExpr

-- | Wraps regular ByteString, but shows and diffs it as hex
data HexBytes = HexBytes
  { hexBytesName :: !(Maybe T.Text)
  , hexBytesRaw :: !BS.ByteString
  }

-- | Ignores the hexBytesName during comparison.
instance Eq HexBytes where
  x1 == x2 = hexBytesRaw x1 == hexBytesRaw x2

named :: (Monoid str, IsString str) => str -> Maybe str -> str
named typeName mName = typeName <> maybe "" (\x -> "<" <> x <> ">") mName

instance Show HexBytes where
  show = showExpr

instance ToExpr HexBytes where
  toExpr HexBytes {..} =
    App (named "HexBytes" (T.unpack <$> hexBytesName)) $ hexByteStringExpr hexBytesRaw

data CBORBytes = CBORBytes
  { cborbytesName :: !(Maybe T.Text)
  , cborBytesRaw :: !BS.ByteString
  }

-- | Ignores the hexBytesName during comparison.
instance Eq CBORBytes where
  x1 == x2 = cborBytesRaw x1 == cborBytesRaw x2

instance Show CBORBytes where
  show = showExpr

instance ToExpr CBORBytes where
  toExpr (CBORBytes name bytes) =
    -- `decodeTerm` does not care about the version, so we can use any version
    case decodeFullDecoder' minBound (named "Term" name) decodeTerm bytes of
      Left err -> error $ "Error decoding CBOR: " ++ showDecoderError err ++ "\n" ++ show bytes
      Right term -> toExpr term

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
      TTagged 24 (TBytes x) -> App "CBOR-in-CBOR" [toExpr (CBORBytes Nothing x)]
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
showHexBytesGrouped bs =
  [ "0x" <> BS8.unpack (BS.take width $ BS.drop i bs16)
  | i <- [0, width .. BS.length bs16 - 1]
  ]
  where
    width = 128
    bs16 = Base16.encode bs

-- | Check that two values are equal and if they are not raise an exception with the
-- `ToExpr` diff
expectExprEqual :: (Eq a, ToExpr a) => a -> a -> Expectation
expectExprEqual x y = expectExprEqualWithMessage "Expected two values to be equal:" x y

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
