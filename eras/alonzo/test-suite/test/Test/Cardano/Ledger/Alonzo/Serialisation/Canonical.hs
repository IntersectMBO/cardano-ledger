{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Canonical (tests) where

import Cardano.Binary
  ( Annotator (..),
    Decoder,
    TokenType (..),
    decodeAnnotator,
    decodeBool,
    decodeBytesCanonical,
    decodeDoubleCanonical,
    decodeIntegerCanonical,
    decodeListLenCanonical,
    decodeMapLenCanonical,
    decodeNull,
    decodeSimpleCanonical,
    decodeStringCanonical,
    peekTokenType,
    serializeEncoding,
    withSlice,
  )
import Cardano.Ledger.Alonzo.Language (Language)
import Cardano.Ledger.Alonzo.PParams
import Control.Monad (replicateM, unless, void)
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as LBS
import Data.Functor.Compose (Compose (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import qualified Test.QuickCheck.Property as QCP
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testProperty "LangDepView encoding is canonical" canonicalLangDepView

canonicalLangDepView :: PParams era -> Set Language -> Property
canonicalLangDepView pparams langs =
  let langViews = Set.fromList $ getLanguageView pparams <$> Set.toList langs
      encodedViews = serializeEncoding $ encodeLangViews langViews
      base16String = show (B16.encode $ LBS.toStrict encodedViews)
   in counterexample base16String $ case isCanonical encodedViews of
        Right () -> QCP.succeeded
        Left message -> QCP.failed {QCP.reason = message}

isCanonical :: LBS.ByteString -> Either String ()
isCanonical bytes =
  case decodeAnnotator "canonicity check" checkCanonicalTerm bytes of
    Left err -> Left (show err)
    Right x -> x

checkCanonicalTerm :: Decoder s (Annotator (Either String ()))
checkCanonicalTerm = do
  tt <- peekTokenType
  let t _ = pure (Right ())
  case tt of
    TypeUInt -> t <$> decodeIntegerCanonical
    TypeUInt64 -> t <$> decodeIntegerCanonical
    TypeNInt -> t <$> decodeIntegerCanonical
    TypeNInt64 -> t <$> decodeIntegerCanonical
    TypeInteger -> t <$> decodeIntegerCanonical
    TypeFloat16 -> t <$> decodeDoubleCanonical
    TypeFloat32 -> t <$> decodeDoubleCanonical
    TypeFloat64 -> t <$> decodeDoubleCanonical
    TypeBytes -> t <$> decodeBytesCanonical
    TypeBytesIndef -> fail "indefinite bytes encoding"
    TypeString -> t <$> decodeStringCanonical
    TypeStringIndef -> fail "indefinite string encoding"
    TypeListLen -> t <$> checkCanonicalList
    TypeListLen64 -> t <$> checkCanonicalList
    TypeListLenIndef -> fail "indefinite list encoding"
    TypeMapLen -> checkCanonicalMap
    TypeMapLen64 -> checkCanonicalMap
    TypeMapLenIndef -> fail "indefinite map encoding"
    -- TypeTag ->
    -- TypeTag64 ->
    TypeBool -> t <$> decodeBool
    TypeNull -> t <$> decodeNull
    TypeSimple -> t <$> decodeSimpleCanonical
    -- TypeBreak ->
    -- TypeInvalid ->
    x -> fail $ "canonicity check for " <> show x <> " not implemented"

{-
- The keys in the map must be sorted as follows:
   -  If two keys have different lengths, the shorter one sorts earlier.
   -  If two keys have the same length, the one with the lower value
      in (byte-wise) lexical order sorts earlier.
-}

shortLex :: ByteString -> ByteString -> Ordering
shortLex x y = case compare (LBS.length x) (LBS.length y) of
  LT -> LT
  GT -> GT
  EQ -> compare (LBS.unpack x) (LBS.unpack y)

checkCanonicalMap :: Decoder s (Annotator (Either String ()))
checkCanonicalMap = do
  n <- decodeMapLenCanonical
  keys <- replicateM n checkCanonicalKVPair
  let keys' :: Annotator (Either String [ByteString])
      keys' = (getCompose . sequenceA . fmap Compose) keys
  pure $
    Annotator $ \fullBytes -> do
      ks <- runAnnotator keys' fullBytes
      unless (isSorted ks) (Left "map keys out of order")

checkCanonicalList :: Decoder s (Annotator (Either String ()))
checkCanonicalList = do
  len <- decodeListLenCanonical
  checkedTerms <- (replicateM len checkCanonicalTerm)
  pure $ void <$> (getCompose . sequenceA . fmap Compose) checkedTerms

isSorted :: [ByteString] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : (xs@(y : _))) = case shortLex x y of
  GT -> False
  _ -> isSorted xs

checkCanonicalKVPair :: Decoder s (Annotator (Either String ByteString))
checkCanonicalKVPair = do
  (key, keyBytes) <- withSlice checkCanonicalTerm
  value <- checkCanonicalTerm
  pure $ getCompose (Compose key *> Compose value *> Compose (Right <$> keyBytes))
