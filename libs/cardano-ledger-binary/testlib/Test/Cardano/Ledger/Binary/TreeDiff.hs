{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.TreeDiff (
  ToExpr (..),
  CBORBytes (..),
  HexBytes (..),
  showExpr,
  ansiExpr,
  ansiExprString,
  diffExpr,
  diffExprString,
  diffExprCompact,
  diffExprCompactString,
  ansiDocToString,
  hexByteStringExpr,
  showHexBytesGrouped,
  assertColorFailure,
  expectExprEqual,
  expectExprEqualWithMessage,
  assertExprEqualWithMessage,
  callStackToLocation,
  srcLocToLocation,
  Expr (App, Rec, Lst),
  defaultExprViaShow,
  trimExprViaShow,
  tableDoc,
  Pretty (..),
  Doc,
  AnsiStyle,
  ansiWlPretty,
  ppEditExpr,
  ediff,
)
where

import qualified Cardano.Binary as Plain
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Crypto.Hash.Class ()
import Cardano.Ledger.Binary
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import Control.Exception (throwIO)
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.IP (IPv4, IPv6)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Text.Lazy as TL
import Data.TreeDiff
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), getCallStack)
import Prettyprinter (Doc)
import qualified Prettyprinter as Pretty
import Prettyprinter.Render.Terminal (AnsiStyle)
import qualified Prettyprinter.Render.Terminal as Pretty
import Test.Cardano.Slotting.TreeDiff ()
import Test.Hspec (Expectation)
import Test.Hspec.Core.Spec (
  FailureReason (ColorizedReason),
  Location (..),
  ResultStatus (Failure),
 )
import Test.Tasty.HUnit (Assertion, assertFailure)

callStackToLocation :: CallStack -> Maybe Location
callStackToLocation cs =
  case getCallStack cs of
    [] -> Nothing
    (_, loc) : _ -> Just $ srcLocToLocation loc

srcLocToLocation :: SrcLoc -> Location
srcLocToLocation loc =
  Location
    { locationFile = srcLocFile loc
    , locationLine = srcLocStartLine loc
    , locationColumn = srcLocStartCol loc
    }

-- | Similar to `assertFailure`, except hspec will not interfer with any escape sequences
-- that indicate color output.
assertColorFailure :: HasCallStack => String -> IO a
assertColorFailure msg =
  throwIO $ Failure (callStackToLocation ?callStack) (ColorizedReason msg)

-- =====================================================
-- Cardano functions that deal with TreeDiff and ToExpr

trimExprViaShow :: Show a => Int -> a -> Expr
trimExprViaShow _n x = defaultExprViaShow x -- App (take n (drop 1 (show x)) ++ "..") []

tableDoc :: Maybe (Doc AnsiStyle) -> [(String, Doc AnsiStyle)] -> Doc AnsiStyle
tableDoc mTitle rows =
  let w = foldr (max . length . fst) 0 rows
      t = case mTitle of
        Just title -> Pretty.hsep ["-----", title, "-----"] <> Pretty.line
        Nothing -> mempty
   in t <> Pretty.vsep [Pretty.fill (w + 1) (Pretty.pretty l) <> r | (l, r) <- rows]

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
showExpr = show . prettyExpr . toExpr

ansiExpr :: ToExpr a => a -> Doc AnsiStyle
ansiExpr = ansiWlExpr . toExpr

ansiExprString :: ToExpr a => a -> String
ansiExprString = ansiDocToString . ansiExpr

diffExpr :: ToExpr a => a -> a -> Doc AnsiStyle
diffExpr x y = ansiWlEditExpr (ediff x y)

diffExprString :: ToExpr a => a -> a -> String
diffExprString x y = ansiDocToString $ diffExpr x y

diffExprCompact :: ToExpr a => a -> a -> Doc AnsiStyle
diffExprCompact x y = ansiWlEditExprCompact (ediff x y)

diffExprCompactString :: ToExpr a => a -> a -> String
diffExprCompactString x y = ansiDocToString $ diffExprCompact x y

ansiDocToString :: Doc AnsiStyle -> String
ansiDocToString = TL.unpack . Pretty.renderLazy . Pretty.layoutPretty Pretty.defaultLayoutOptions

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
  , Lst (map toExpr $ showHexBytesGrouped 128 bs)
  ]

-- | Show a ByteString as hex groups of 8bytes each. This is a slightly more
-- useful form for debugging, rather than bunch of escaped characters.
showHexBytesGrouped :: Int -> BS.ByteString -> [String]
showHexBytesGrouped n bs
  | BS.null bs = []
  | otherwise =
      [ BS8.unpack (BS.take n $ BS.drop i bs16)
      | i <- [0, n .. BS.length bs16 - 1]
      ]
  where
    bs16 = Base16.encode bs

-- | Check that two values are equal and if they are not raise an exception with the
-- `ToExpr` diff
expectExprEqual :: (Eq a, ToExpr a) => a -> a -> Expectation
expectExprEqual = expectExprEqualWithMessage "Expected two values to be equal:"

-- | Use this with HSpec, but with Tasty use 'assertExprEqualWithMessage' below
expectExprEqualWithMessage :: (ToExpr a, Eq a, HasCallStack) => String -> a -> a -> Expectation
expectExprEqualWithMessage = requireExprEqualWithMessage (assertColorFailure . ansiDocToString) . Pretty.pretty

-- | Use this with Tasty, but with HSpec use 'expectExprEqualWithMessage' above
assertExprEqualWithMessage :: (ToExpr a, Eq a, HasCallStack) => String -> a -> a -> Assertion
assertExprEqualWithMessage = requireExprEqualWithMessage (assertFailure . ansiDocToString) . Pretty.pretty

requireExprEqualWithMessage ::
  (ToExpr a, Eq a, Monoid b) => (Doc AnsiStyle -> b) -> Doc AnsiStyle -> a -> a -> b
requireExprEqualWithMessage fail_ message expected actual =
  if actual == expected then mempty else fail_ doc
  where
    doc = Pretty.width message (\w -> if w == 0 then diff else Pretty.line <> Pretty.indent 2 diff)
    diff = diffExpr expected actual
