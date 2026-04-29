{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Huddle.Gen (
  -- * MonadGen
  module GenT,

  -- * CBORGen
  module CBORGen,

  -- * Term generators
  Term (..),
  WrappedTerm (..),
  Name (..),
  genRule,
  generateFromName,
  generateFromGRef,
  genArrayTerm,
  genBytesTerm,
  genStringTerm,
  genMapTerm,

  -- * Term validators
  validateFromName,
  validateFromGRef,
  validateInt,
  validateUInt,
  validateNInt,
  validateArrayTerm,
  validateBytesTerm,
  validateStringTerm,
  validateMapTerm,
  unwrapSingle,

  -- * Lifted generators
  arbitrary,
  scale,
  shuffle,

  -- * Antigen
  module AntiGen,
) where

import Cardano.Ledger.Binary (Term (..))
import Cardano.Ledger.Huddle (HuddleRule ())
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromGRef, generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateFromGRef, validateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CBORGenerator as CBORGen
import Codec.CBOR.Cuddle.CDDL.CTree (nintMin, uintMax)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import GHC.TypeLits (symbolVal)
import Test.AntiGen as AntiGen
import qualified Test.QuickCheck as QC
import Test.QuickCheck.GenT as GenT

-- | A function for generating a term from a rule. The @HuddleRule@ constraint
-- ensures that the rule is actually defined in that era.
genRule :: forall rule era. HuddleRule rule era => CBORGen Term
genRule = generateFromName (Name . T.pack . symbolVal $ Proxy @rule)

-- Lifted Gen functions

arbitrary :: forall a m. (MonadGen m, QC.Arbitrary a) => m a
arbitrary = liftGen QC.arbitrary

scale :: MonadGen m => (Int -> Int) -> m a -> m a
scale f m = sized $ \sz -> resize (f sz) m

shuffle :: MonadGen m => [a] -> m [a]
shuffle = liftGen . QC.shuffle

-- Term generators

genArrayTerm :: MonadGen m => [Term] -> m Term
genArrayTerm es = GenT.elements [TList es, TListI es]

genBytesTerm :: MonadGen m => ByteString -> m Term
genBytesTerm bs = GenT.elements [TBytes bs, TBytesI $ LBS.fromStrict bs]

genStringTerm :: MonadGen m => T.Text -> m Term
genStringTerm t = GenT.elements [TString t, TStringI $ LT.fromStrict t]

genMapTerm :: MonadGen m => [(Term, Term)] -> m Term
genMapTerm m = GenT.elements [TMap m, TMapI m]

-- Term validators

validateInt :: Term -> CBORValidator Integer
validateInt (TInt (toInteger -> x))
  | x >= nintMin || x <= uintMax = pure x
  | otherwise = fail "Number not in int range"
validateInt _ = fail "Expected int"

validateUInt :: Term -> CBORValidator Integer
validateUInt (TInt (toInteger -> x))
  | x >= 0 || x <= uintMax = pure x
  | otherwise = fail "Number not in uint range"
validateUInt _ = fail "Expected uint"

validateNInt :: Term -> CBORValidator Integer
validateNInt (TInt (toInteger -> x))
  | x >= nintMin || x < 0 = pure x
  | otherwise = fail "Number not in nint range"
validateNInt _ = fail "Expected nint"

validateArrayTerm :: Term -> CBORValidator [Term]
validateArrayTerm (TList xs) = pure xs
validateArrayTerm (TListI xs) = pure xs
validateArrayTerm _ = fail "Expected list"

validateBytesTerm :: Term -> CBORValidator ByteString
validateBytesTerm (TBytes bs) = pure bs
validateBytesTerm (TBytesI bs) = pure $ LBS.toStrict bs
validateBytesTerm _ = fail "Expected bytes"

validateStringTerm :: Term -> CBORValidator Text
validateStringTerm (TString x) = pure x
validateStringTerm (TStringI x) = pure $ LT.toStrict x
validateStringTerm _ = fail "Expected string"

validateMapTerm :: Term -> CBORValidator [(Term, Term)]
validateMapTerm (TMap xs) = pure xs
validateMapTerm (TMapI xs) = pure xs
validateMapTerm _ = fail "Expected map"

unwrapSingle :: WrappedTerm -> CBORValidator Term
unwrapSingle (S x) = pure x
unwrapSingle _ = fail "Expected a single term"
