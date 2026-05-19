{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Huddle.Gen (
  -- * MonadGen
  module GenT,

  -- * Custom core
  module CustomCore,

  -- * Term generators
  module CustomGen,
  Term (..),
  RuleTerm (..),
  Name (..),
  genRule,
  generateFromName,
  generateFromGRef,
  genArrayTerm,
  genBytesTerm,
  genStringTerm,
  genMapTerm,
  unwrapSingleOrError,

  -- * Term validators
  module CustomValidator,
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
  antiVectorOfUnique,
  antiVectorOfUniqueBy,
  antiVectorOfUniqueOn,
) where

import Cardano.Ledger.Binary (Term (..))
import Cardano.Ledger.Huddle (HuddleRule ())
import Codec.CBOR.Cuddle.CBOR.Gen (generateFromGRef, generateFromName)
import Codec.CBOR.Cuddle.CBOR.Validator (validateFromGRef, validateFromName)
import Codec.CBOR.Cuddle.CDDL (Name (..))
import Codec.CBOR.Cuddle.CDDL.CTree (nintMin, uintMax)
import Codec.CBOR.Cuddle.CDDL.Custom.Core as CustomCore
import Codec.CBOR.Cuddle.CDDL.Custom.Generator as CustomGen
import Codec.CBOR.Cuddle.CDDL.Custom.Validator as CustomValidator
import Control.Monad.Reader (asks)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
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

-- | Generate a list of @n@ pairwise-distinct elements. Returns 'Nothing' if
-- the underlying generator could not produce enough distinct elements within
-- the per-element retry budget.
antiVectorOfUnique :: Eq a => Int -> AntiGen a -> AntiGen (Maybe [a])
antiVectorOfUnique = antiVectorOfUniqueBy (==)

-- | Like 'antiVectorOfUnique', but compares elements by a key projection.
antiVectorOfUniqueOn :: Eq b => (a -> b) -> Int -> AntiGen a -> AntiGen (Maybe [a])
antiVectorOfUniqueOn key = antiVectorOfUniqueBy ((==) `on` key)

-- | Like 'antiVectorOfUnique', but takes a user-supplied equivalence relation.
antiVectorOfUniqueBy :: (a -> a -> Bool) -> Int -> AntiGen a -> AntiGen (Maybe [a])
antiVectorOfUniqueBy eq n gen = do
  disallowDuplicates <- faultyBool True
  let
    triesPerElement = 10 :: Int
    go _ 0 _ = pure Nothing
    go m tries elems
      | m > 0 = do
          x <- gen
          if disallowDuplicates && any (eq x) elems
            then go m (tries - 1) elems
            else go (m - 1) triesPerElement (x : elems)
      | otherwise = pure (Just elems)
  go n triesPerElement []

genArrayTerm :: [Term] -> CBORGen Term
genArrayTerm es =
  ifTwiddle (GenT.elements [TList es, TListI es]) (pure $ TList es)

genBytesTerm :: ByteString -> CBORGen Term
genBytesTerm bs =
  ifTwiddle (GenT.elements [TBytes bs, TBytesI $ LBS.fromStrict bs]) (pure $ TBytes bs)

genStringTerm :: T.Text -> CBORGen Term
genStringTerm t =
  ifTwiddle (GenT.elements [TString t, TStringI $ LT.fromStrict t]) (pure $ TString t)

genMapTerm :: [(Term, Term)] -> CBORGen Term
genMapTerm m =
  ifTwiddle (GenT.elements [TMap m, TMapI m]) (pure $ TMap m)

ifTwiddle :: CBORGen a -> CBORGen a -> CBORGen a
ifTwiddle yes no = do
  twiddle <- asks (gcTwiddle . geConfig)
  if twiddle then yes else no

-- Term validators

validateInt :: Term -> Validator Integer
validateInt (TInt (toInteger -> x))
  | x >= nintMin && x <= uintMax = pure x
  | otherwise = fail "Number not in int range"
validateInt _ = fail "Expected int"

validateUInt :: Term -> Validator Integer
validateUInt (TInt (toInteger -> x))
  | x >= 0 && x <= uintMax = pure x
  | otherwise = fail "Number not in uint range"
validateUInt _ = fail "Expected uint"

validateNInt :: Term -> Validator Integer
validateNInt (TInt (toInteger -> x))
  | x >= nintMin && x < 0 = pure x
  | otherwise = fail "Number not in nint range"
validateNInt _ = fail "Expected nint"

validateArrayTerm :: Term -> Validator [Term]
validateArrayTerm (TList xs) = pure xs
validateArrayTerm (TListI xs) = pure xs
validateArrayTerm _ = fail "Expected list"

validateBytesTerm :: Term -> Validator ByteString
validateBytesTerm (TBytes bs) = pure bs
validateBytesTerm (TBytesI bs) = pure $ LBS.toStrict bs
validateBytesTerm _ = fail "Expected bytes"

validateStringTerm :: Term -> Validator Text
validateStringTerm (TString x) = pure x
validateStringTerm (TStringI x) = pure $ LT.toStrict x
validateStringTerm _ = fail "Expected string"

validateMapTerm :: Term -> Validator [(Term, Term)]
validateMapTerm (TMap xs) = pure xs
validateMapTerm (TMapI xs) = pure xs
validateMapTerm _ = fail "Expected map"

unwrapSingle :: RuleTerm -> Validator Term
unwrapSingle (SingleTerm x) = pure x
unwrapSingle _ = fail "Expected a single term"

unwrapSingleOrError :: RuleTerm -> Term
unwrapSingleOrError (SingleTerm x) = x
unwrapSingleOrError _ = error "Expected a single term"
