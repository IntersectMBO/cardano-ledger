{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Binary.Twiddle (
  Twiddler (..),
  Twiddle (..),
  Twiddle1 (..),
  -- Utils
  encodingToTerm,
  toTwiddler,
  toTerm,
  emptyOrNothing,
  twiddleStrictMaybe,
  twiddleTerm,
  -- Testing
  twiddleInvariantProp,
) where

import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  Encoding,
  Term (..),
  Version,
  decodeFull,
  encodeTerm,
  getDecoderVersion,
  serialize,
 )
import Data.Bitraversable (bimapM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Ratio (Ratio, denominator, numerator)
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Numeric.Natural (Natural)
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.QuickCheck (Arbitrary (..), Gen, Property, elements, oneof, shuffle, (===))

data Twiddler a = Twiddler
  { twiddlerVersion :: !Version
  , twiddlerType :: !a
  , twiddlerTerm :: Term
  }

instance (Twiddle a, Arbitrary a, EncCBOR a) => Arbitrary (Twiddler a) where
  arbitrary = do
    x <- arbitrary
    v <- arbitrary
    enc' <- twiddle v x
    pure $ Twiddler v x enc'

instance Typeable a => EncCBOR (Twiddler a) where
  encCBOR (Twiddler _ _ x) = encodeTerm x

instance (EncCBOR a, DecCBOR a) => DecCBOR (Twiddler a) where
  decCBOR = do
    v <- getDecoderVersion
    (\x -> Twiddler v x $ toTerm v x) <$> decCBOR

instance Show a => Show (Twiddler a) where
  show (Twiddler v x _) = "Twiddler " <> show v <> ": " <> show x

instance Eq a => Eq (Twiddler a) where
  Twiddler v1 x1 _ == Twiddler v2 x2 _ = v1 == v2 && x1 == x2

gTwiddleTList :: forall a p. (Generic a, TwiddleL (Rep a p)) => Version -> a -> Gen Term
gTwiddleTList v a = TList <$> twiddleL v (from @a @p a)

-- | Introducing random variations into the CBOR encoding of a value while
--   preserving the round-trip properties.
--
--   For any value `x :: a`, where `a` derives `Twiddle`, and for any version
--   of the decoder, the following property must hold:
--   >>> fmap ((== x) . encodingToTerm version . encCBOR) (twiddle x)
class Twiddle a where
  -- | Given a value of type `a`, generates a CBOR `Term` that can contain
  -- slight variations without changing the semantics. After encoding and
  -- decoding the `Term`, we should get back the original value that was being
  -- twiddled.
  --
  -- In addition to varying the low-level CBOR tokens, `twiddle` can also
  -- be used to introduce higher level variations. For example if the schema
  -- of a value allows a field to be either an empty list or absent
  -- entirely, and both are interpreted the same way, then `twiddle`
  -- can be used to randomly pick either of these representations.
  twiddle :: Version -> a -> Gen Term
  default twiddle :: forall p. (Generic a, TwiddleL (Rep a p)) => Version -> a -> Gen Term
  twiddle = gTwiddleTList @a @p

instance Twiddle Void where
  twiddle _ = absurd

instance Twiddle () where
  twiddle = twiddleTerm

instance Twiddle Natural where
  twiddle _ = twiddleIntegral

instance Twiddle Word where
  twiddle _ = twiddleIntegral

instance Twiddle Word8 where
  twiddle _ = twiddleIntegral

instance Twiddle Word16 where
  twiddle _ = twiddleIntegral

instance Twiddle Word32 where
  twiddle _ = twiddleIntegral

instance Twiddle Word64 where
  twiddle _ = twiddleIntegral

instance Twiddle Int8 where
  twiddle _ = twiddleIntegral

instance Twiddle Int16 where
  twiddle _ = twiddleIntegral

instance Twiddle Int32 where
  twiddle _ = twiddleIntegral

instance Twiddle Int64 where
  twiddle _ = twiddleIntegral

instance Twiddle a => Twiddle (Ratio a) where
  twiddle = liftTwiddle twiddle

instance Twiddle a => Twiddle (Maybe a) where
  twiddle = liftTwiddle twiddle

instance Twiddle a => Twiddle [a] where
  twiddle = liftTwiddle twiddle

instance (Twiddle k, Twiddle v) => Twiddle (Map k v) where
  twiddle v m = do
    -- Elements of a map do not have to be in a specific order so we shuffle them
    m' <- shuffle $ Map.toList m
    m'' <- traverse (bimapM (twiddle v) (twiddle v)) m'
    f <- elements [TMap, TMapI]
    pure $ f m''

instance Twiddle ByteString where
  twiddle _ bs = do
    f <- elements [TBytes, TBytesI . fromStrict]
    pure $ f bs

instance Twiddle Text where
  twiddle _ t = do
    f <- elements [TString, TStringI . T.fromStrict]
    pure $ f t

instance Twiddle Int where
  -- TODO: Put small ints into bigger words (e.g. a Word16 value into Word32)
  --
  -- This is not possible with the CBOR AST provided by cborg
  twiddle _ = pure . TInt

instance Twiddle a => Twiddle (Set a) where
  twiddle = liftTwiddle twiddle

instance Twiddle a => Twiddle (Seq a) where
  twiddle = liftTwiddle twiddle

instance Twiddle a => Twiddle (StrictSeq a) where
  twiddle = liftTwiddle twiddle

class TwiddleL a where
  twiddleL :: Version -> a -> Gen [Term]

instance TwiddleL (V1 p) where
  twiddleL _ v1 = case v1 of {}

instance TwiddleL (U1 p) where
  twiddleL _ U1 = pure []

instance (TwiddleL (l x), TwiddleL (r x)) => TwiddleL ((l :*: r) x) where
  twiddleL v (lx :*: rx) = do
    lx' <- twiddleL v lx
    rx' <- twiddleL v rx
    pure $ lx' <> rx'

instance (TwiddleL (l x), TwiddleL (r x)) => TwiddleL ((l :+: r) x) where
  twiddleL v (L1 lx) = twiddleL v lx
  twiddleL v (R1 rx) = twiddleL v rx

instance Twiddle c => TwiddleL (K1 i c p) where
  twiddleL v (K1 c) = pure <$> twiddle v c

instance TwiddleL (f p) => TwiddleL (M1 i c f p) where
  twiddleL v (M1 fp) = twiddleL v fp

instance Twiddle Integer where
  twiddle _ = pure . TInteger

instance Twiddle Bool where
  twiddle _ = pure . TBool

instance Twiddle Float where
  twiddle _ = pure . TFloat

instance Twiddle Double where
  twiddle _ = pure . TDouble

instance Twiddle Term where
  twiddle v (TInt n) = twiddle v n
  twiddle v (TInteger n) = twiddle v n
  twiddle v (TBytes bs) = twiddle v bs
  twiddle v (TBytesI bs) = twiddle v $ toStrict bs
  twiddle v (TString txt) = twiddle v txt
  twiddle v (TStringI txt) = twiddle v $ T.toStrict txt
  twiddle v (TList tes) = twiddle v tes
  twiddle v (TListI tes) = twiddle v tes
  twiddle v (TMap x0) = twiddle v $ Map.fromList x0
  twiddle v (TMapI x0) = twiddle v $ Map.fromList x0
  twiddle v (TTagged wo te') = TTagged wo <$> twiddle v te'
  twiddle v (TBool b) = twiddle v b
  twiddle _ TNull = pure TNull
  twiddle _ (TSimple wo) = pure $ TSimple wo
  twiddle _ (THalf x) = pure $ THalf x
  twiddle v (TFloat x) = twiddle v x
  twiddle v (TDouble x) = twiddle v x

class Twiddle1 f where
  liftTwiddle :: (Version -> a -> Gen Term) -> Version -> f a -> Gen Term

instance Twiddle1 [] where
  liftTwiddle f v l = do
    lt <- elements [TList, TListI]
    l' <- traverse (f v) l
    pure $ lt l'

liftTwiddleViaList :: Foldable t => (Version -> a -> Gen Term) -> Version -> t a -> Gen Term
liftTwiddleViaList f v x = liftTwiddle f v $ toList x

instance Twiddle1 Maybe where
  liftTwiddle = liftTwiddleViaList

instance Twiddle1 StrictMaybe where
  liftTwiddle = liftTwiddleViaList

instance Twiddle1 Ratio where
  liftTwiddle f v x = liftTwiddle f v [numerator x, denominator x]

instance Twiddle1 Set where
  liftTwiddle = liftTwiddleViaList

instance Twiddle1 Seq where
  liftTwiddle = liftTwiddleViaList

instance Twiddle1 StrictSeq where
  liftTwiddle = liftTwiddleViaList

-- * Utility functions

encodingToTerm :: Version -> Encoding -> Term
encodingToTerm version enc =
  case decodeFull version (serialize version enc) of
    Right t -> t
    Left err -> error $ show err

toTerm :: EncCBOR a => Version -> a -> Term
toTerm version = encodingToTerm version . encCBOR

-- | Wraps an arbitrary value into a `Twiddler`
toTwiddler :: Twiddle a => Version -> a -> Gen (Twiddler a)
toTwiddler v x = Twiddler v x <$> twiddle v x

-- | Optional containers have two "empty" representations. One of
-- them is to return an empty container and the other is to omit the field.
-- This utility function randomly picks one of these representations, where
-- omission is represented by `Nothing` and empty container is returned with
-- `Just`. These values can then be easily concatenated with `catMaybes`.
emptyOrNothing ::
  forall t b.
  ( Foldable t
  , Twiddle (t Void)
  , Monoid (t Void)
  , Twiddle (t b)
  ) =>
  Version ->
  t b ->
  Gen (Maybe Term)
emptyOrNothing v x =
  if null x
    then
      oneof
        [ Just <$> twiddle @(t Void) v mempty
        , pure Nothing
        ]
    else Just <$> twiddle v x

-- | Utility function for twiddling optional fields. It works similarly to
-- the twiddle method of StrictMaybe, but lifts the Maybe constructor out from
-- the term so it can be easily concatenated with `catMaybes`.
twiddleStrictMaybe :: Twiddle a => Version -> StrictMaybe a -> Gen (Maybe Term)
twiddleStrictMaybe _ SNothing = pure Nothing
twiddleStrictMaybe v (SJust x) = Just <$> twiddle v x

-- | Twiddle a value by first encoding it as a `Term` and then twiddling that
twiddleTerm :: EncCBOR a => Version -> a -> Gen Term
twiddleTerm v = twiddle v . toTerm v

-- TODO also try with TInt if value is small enough
twiddleIntegral :: Integral a => a -> Gen Term
twiddleIntegral = pure . TInteger . fromIntegral

-- * Testing

-- | Function for testing the invariant of a `Twiddle` instance. For a correct
-- implementation, this property should always hold.
twiddleInvariantProp :: forall a. Twiddle a => Version -> a -> Gen Property
twiddleInvariantProp version x = do
  t <- twiddle version x
  let t' = encodingToTerm version $ encCBOR t
  pure $ t === t'
