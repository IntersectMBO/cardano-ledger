{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.TreeDiff
  ( CBORBytes (..),
    HexBytes (..),
    showExpr,
    diffExpr,
    expectExprEqual,
    hexByteStringExpr,
    showHexBytesGrouped,
  )
where

import Cardano.Ledger.Binary
import Control.Monad
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.TreeDiff
import Test.Hspec

--------------------------------------------------------------------------------
--  Diffing and pretty showing CBOR
--------------------------------------------------------------------------------

diffExpr :: ToExpr a => a -> a -> String
diffExpr x y = show (ansiWlEditExpr (ediff x y))

showExpr :: ToExpr a => a -> String
showExpr = show . ansiWlExpr . toExpr

-- | Wraps regular ByteString, but shows and diffs it as hex
newtype HexBytes = HexBytes {unHexBytes :: BS.ByteString}
  deriving (Eq)

instance Show HexBytes where
  show = showExpr

instance ToExpr HexBytes where
  toExpr = App "HexBytes" . hexByteStringExpr . unHexBytes

-- | Check that two values are equal and if they are not raise an exception with the
-- `ToExpr` diff
expectExprEqual :: (Eq a, ToExpr a) => a -> a -> Expectation
expectExprEqual x y =
  unless (x == y) $
    expectationFailure
      ("Expected for two values to be equal:\n" ++ diffExpr x y)

newtype CBORBytes = CBORBytes {unCBORBytes :: BS.ByteString}
  deriving (Eq)

instance Show CBORBytes where
  show = showExpr

instance ToExpr CBORBytes where
  toExpr (CBORBytes bytes) =
    -- `decodeTerm` does not care about the version, so we can use any version
    case decodeFullDecoder' minBound "Term" decodeTerm bytes of
      Left err -> error $ "Error decoding CBOR: " ++ showDecoderError err
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
  [ toExpr (BS.length bs),
    Lst (map toExpr $ showHexBytesGrouped bs)
  ]

-- | Show a ByteString as hex groups of 8bytes each. This is a slightly more
-- useful form for debugging, rather than bunch of escaped characters.
showHexBytesGrouped :: BS.ByteString -> [String]
showHexBytesGrouped bs =
  [ "0x" <> BS8.unpack (BS.take 16 $ BS.drop i bs16)
    | i <- [0, 16 .. BS.length bs16 - 1]
  ]
  where
    bs16 = Base16.encode bs
