{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyCase #-}

module Data.Twiddle
  ( Twiddler (unTwiddler),
    Twiddle (..),
    toTwiddler,
    encodingToTerm,
    toTerm,
  )
where

import Cardano.Binary (ToCBOR (..), FromCBOR (..), Encoding)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toLazyByteString)
import Data.Bitraversable (bimapM)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Data.Typeable (Typeable)
import GHC.Generics
import Test.QuickCheck (Arbitrary (..), Gen, elements, shuffle)
import Data.Void (Void, absurd)

data Twiddler a = Twiddler
  { unTwiddler :: !a,
    _unEnc :: Term
  }

gTwiddleTList :: forall a p. (Generic a, TwiddleL' (Rep a p)) => a -> Gen Term
gTwiddleTList a = TList <$> twiddleL' (from @a @p a)

class Twiddle a where
  twiddle :: a -> Gen Term
  default twiddle :: forall p. (Generic a, TwiddleL' (Rep a p)) => a -> Gen Term
  twiddle = gTwiddleTList @a @p

instance Twiddle a => Twiddle [a] where
  twiddle l = do
    f <- elements [TList, TListI]
    l' <- traverse twiddle l
    pure $ f l'

instance (Twiddle k, Twiddle v) => Twiddle (Map k v) where
  twiddle m = do
    -- Elements of a map do not have to be in a specific order so we shuffle them
    m' <- shuffle $ Map.toList m
    m'' <- traverse (bimapM twiddle twiddle) m'
    f <- elements [TMap, TMapI]
    pure $ f m''

instance Twiddle ByteString where
  twiddle bs = do
    f <- elements [TBytes, TBytesI . fromStrict]
    pure $ f bs

instance Twiddle Text where
  twiddle t = do
    f <- elements [TString, TStringI . T.fromStrict]
    pure $ f t

instance Twiddle Int where
  -- TODO: Put small ints into bigger words (e.g. a Word16 value into Word32)
  --
  -- This is not possible with the CBOR AST provided by cborg
  twiddle = pure . TInt

instance (Twiddle a, Arbitrary a, ToCBOR a) => Arbitrary (Twiddler a) where
  arbitrary = do
    x <- arbitrary
    enc' <- twiddle x
    pure $ Twiddler x enc'

instance Twiddle a => Twiddle (Set a) where
  twiddle = twiddle . toList

instance Twiddle a => Twiddle (Seq a) where
  twiddle = twiddle . toList

instance Twiddle a => Twiddle (StrictSeq a) where
  twiddle = twiddle . toList

instance Typeable a => ToCBOR (Twiddler a) where
  toCBOR (Twiddler _ x) = encodeTerm x

instance (Typeable a, ToCBOR a, FromCBOR a) => FromCBOR (Twiddler a) where
  fromCBOR = f <$> fromCBOR
    where
      f x = Twiddler x $ toTerm x

instance Show a => Show (Twiddler a) where
  show (Twiddler x _) = "Twiddler " <> show x

instance Eq a => Eq (Twiddler a) where
  (Twiddler x _) == (Twiddler y _) = x == y

class TwiddleL' a where
  twiddleL' :: a -> Gen [Term]

instance TwiddleL' (V1 p) where
  twiddleL' v1 = case v1 of

instance TwiddleL' (U1 p) where
  twiddleL' U1 = pure []

instance (TwiddleL' (l x), TwiddleL' (r x)) => TwiddleL' ((l :*: r) x) where
  twiddleL' (lx :*: rx) = do
    lx' <- twiddleL' lx
    rx' <- twiddleL' rx
    pure $ lx' <> rx'

instance (TwiddleL' (l x), TwiddleL' (r x)) => TwiddleL' ((l :+: r) x) where
  twiddleL' (L1 lx) = twiddleL' lx
  twiddleL' (R1 rx) = twiddleL' rx

instance Twiddle c => TwiddleL' (K1 i c p) where
  twiddleL' (K1 c) = pure <$> twiddle c

instance (TwiddleL' (f p)) => TwiddleL' (M1 i c f p) where
  twiddleL' (M1 fp) = twiddleL' fp

instance Twiddle Integer where
  twiddle = pure . TInteger

instance Twiddle Void where
  twiddle = absurd

instance Twiddle Bool where
  twiddle = pure . TBool

instance Twiddle Float where
  twiddle = pure . TFloat

instance Twiddle Double where
  twiddle = pure . TDouble

instance Twiddle Term where
  twiddle (TInt n) = twiddle n
  twiddle (TInteger n) = twiddle n
  twiddle (TBytes bs) = twiddle bs
  twiddle (TBytesI bs) = twiddle $ toStrict bs
  twiddle (TString txt) = twiddle txt
  twiddle (TStringI txt) = twiddle $ T.toStrict txt
  twiddle (TList tes) = twiddle tes
  twiddle (TListI tes) = twiddle tes
  twiddle (TMap x0) = twiddle $ Map.fromList x0
  twiddle (TMapI x0) = twiddle $ Map.fromList x0
  twiddle (TTagged wo te') = TTagged wo <$> twiddle te'
  twiddle (TBool b) = twiddle b
  twiddle TNull = pure TNull
  twiddle (TSimple wo) = pure $ TSimple wo
  twiddle (THalf x) = pure $ THalf x
  twiddle (TFloat x) = twiddle x
  twiddle (TDouble x) = twiddle x

encodingToTerm :: Encoding -> Term
encodingToTerm enc = case res of
  Right (_, t) -> t
  Left err -> error $ show err
  where
    res = deserialiseFromBytes decodeTerm $ toLazyByteString enc

toTerm :: ToCBOR a => a -> Term
toTerm = encodingToTerm . toCBOR

toTwiddler :: Twiddle a => a -> Gen (Twiddler a)
toTwiddler x = Twiddler x <$> twiddle x
