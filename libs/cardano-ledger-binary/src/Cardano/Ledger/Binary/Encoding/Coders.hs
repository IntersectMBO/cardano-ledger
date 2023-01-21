{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Data.Coders provides tools for writing 'ToCBOR' and 'FromCBOR' instances (see module
--   'Cardano.Binary') in an intuitive way that mirrors the way one constructs values of a
--   particular type. Advantages include:
--
-- 1. Book-keeping details neccesary to write correct instances are hidden from the user.
-- 2. Inverse 'ToCBOR' and 'FromCBOR' instances have visually similar definitions.
-- 3. Advanced instances involving sparse-encoding, compact-representation, and 'Annotator' instances are also supported.
--
-- A Guide to Visual inspection of Duality in Encode and Decode
--
-- 1. @(Sum c)@     and @(SumD c)@    are duals
-- 2. @(Rec c)@     and @(RecD c)@    are duals
-- 3. @(Keyed c)@   and @(KeyedD c)@  are duals
-- 4. @(OmitC x)@   and @(Emit x)@    are duals
-- 5. @(Omit p ..)@ and @(Emit x)@    are duals if (p x) is True
-- 6. @(To x)@      and @(From)@      are duals if (x::T) and (forall (y::T). isRight (roundTrip y))
-- 7. @(E enc x)@   and @(D dec)@     are duals if (forall x . isRight (roundTrip' enc dec x))
-- 6. @(ED d x)@    and @(DD f)@      are duals as long as d=(Dual enc dec) and (forall x . isRight (roundTrip' enc dec x))
-- 7. @(f !> x)@    and @(g <! y)@    are duals if (f and g are duals) and (x and y are duals)
--
-- Duality properties of @(Summands name decodeT)@ and @(SparseKeyed name (init::T) pick required)@ also exist
-- but are harder to describe succinctly.
module Cardano.Ledger.Binary.Encoding.Coders (
  -- * Creating encoders.

  --
  -- $Encoders
  Encode (..),
  (!>),
  encode,

  -- * Index types for well-formed Coders.

  --
  -- $Indexes
  runE, -- Used in testing
  encodeDual,

  -- * Containers, Combinators, Annotators

  --
  -- $Combinators
  encodeKeyedStrictMaybeWith,
  encodeKeyedStrictMaybe,
)
where

import Cardano.Ledger.Binary.Decoding.Coders (Density (Dense, Sparse), Wrapped (..))
import Cardano.Ledger.Binary.Decoding.Decoder (Decoder)
import Cardano.Ledger.Binary.Decoding.FromCBOR (FromCBOR (..))
import Cardano.Ledger.Binary.Encoding.Encoder (
  Encoding,
  encodeListLen,
  encodeMapLen,
  encodeTag,
  encodeWord,
  fromPlainEncoding,
 )
import Cardano.Ledger.Binary.Encoding.ToCBOR (ToCBOR (toCBOR))
import Cardano.Ledger.Binary.Plain (EncCBOR (..))
import Data.Maybe.Strict (StrictMaybe (SJust, SNothing))

-- ====================================================================

-- ===============================================================================
-- Encode and Decode are typed data structures which specify encoders and decoders
-- for Algebraic data structures written in Haskell. They exploit types and count
-- the correct number fields in an encoding and decoding, which are automatically computed.
-- They are somewhat dual, and are designed so that visual inspection of a Encode and
-- its dual Decode can help the user conclude that the two are self-consistent.
-- They are also reusable abstractions that can be defined once, and then used many places.
--
-- (Encode t) is a data structure from which 3 things can be recovered
-- Given:    x :: Encode t
-- 1. get a value of type t
-- 2. get an Encoding for that value, which correctly encodes the number of "fields"
--    written to the ByteString. Care must still be taken that the Keys are correct.
-- 3. get a (MemoBytes t)
--
-- The advantage of using Encode with a MemoBytes, is we don't have to make a ToCBOR
-- instance. Instead the "instance" is spread amongst the pattern constuctors by using
-- (memoBytes encoding) in the where clause of the pattern contructor.
-- See some examples of this see the file Timelocks.hs
--

-- ===========================================================
-- The coders and the decoders as GADT datatypes
-- ===========================================================

-- Encoders

-- | A first-order domain specific langage for describing ToCBOR instances. Applying
--   the interpreter 'encode' to a well-typed @(Encode w T)@ always produces a valid encoding for @T@.
--   Constructing an Encode of type T is just like building a value of type T, applying a constructor
--   of @T@ to the correctly typed arguments. For example
--
-- @
-- data T = T Bool Word
--
-- instance ToCBOR T where
--   toCBOR (T b w) = encode (Rec T !> To b !> To w)
-- @
--
-- Note the similarity of
--
-- @(/T/ /b/ /w/)@ and @(/T/ $ /b/ $ /w/)@ and @(Rec /T/ !> To /b/ !> To /w/)@
--
-- Where ('!>') is the infx version of 'ApplyE' with the same infixity and precedence as ('$'). Note
-- how the constructor and each (component, field, argument) is labeled with one of the constructors
-- of 'Encode', and are combined with the application operator ('!>'). Using different constructors supports
-- different styles of encoding.
data Encode (w :: Wrapped) t where
  -- | Label the constructor of a Record-like datatype (one with exactly 1 constructor) as an Encode.
  Rec :: t -> Encode ('Closed 'Dense) t
  -- | Label one of the constructors of a sum datatype (one with multiple constructors) as an Encode
  Sum :: t -> Word -> Encode 'Open t
  -- | Label the constructor of a Record-like datatype as being encoded sparsely (storing only non-default values).
  Keyed :: t -> Encode ('Closed 'Sparse) t
  -- | Label an (component, field, argument) to be encoded using an existing ToCBOR instance.
  To :: ToCBOR a => a -> Encode ('Closed 'Dense) a
  -- | Label an (component, field, argument) to be encoded using an existing ToCBOR instance.
  Enc :: EncCBOR a => a -> Encode ('Closed 'Dense) a
  -- | Label a  (component, field, argument) to be encoded using the given encoding function.
  E :: (t -> Encoding) -> t -> Encode ('Closed 'Dense) t
  -- | Lift one Encode to another with a different type. Used to make a Functor instance of (Encode w).
  MapE :: (a -> b) -> Encode w a -> Encode w b
  -- | Skip over the  (component,field, argument), don't encode it at all (used in sparse encoding).
  OmitC :: t -> Encode w t
  -- | Precede the given encoding (in the produced bytes) with the given tag Word.
  Tag :: Word -> Encode ('Closed x) t -> Encode ('Closed x) t
  -- | Omit the  (component,field, argument) if the function is True, otherwise encode with the given encoding.
  Omit ::
    (t -> Bool) ->
    Encode ('Closed 'Sparse) t ->
    Encode ('Closed 'Sparse) t
  -- | Precede the encoding (in the produced bytes) with the key Word. Analagous to 'Tag', but lifts a 'Dense' encoding to a 'Sparse' encoding.
  Key :: Word -> Encode ('Closed 'Dense) t -> Encode ('Closed 'Sparse) t
  -- | Apply a functional encoding (arising from 'Rec' or 'Sum') to get (type wise) smaller encoding. A fully saturated chain of 'ApplyE' will be a complete encoding. See also '!>' which is infix 'ApplyE'.
  ApplyE :: Encode w (a -> t) -> Encode ('Closed r) a -> Encode w t

-- The Wrapped index of ApplyE is determined by the index
-- at the bottom of its left spine. The choices are 'Open (Sum c tag),
-- ('Closed 'Dense) (Rec c), and ('Closed 'Sparse) (Keyed c).

infixl 4 !>

-- | Infix operator version of @ApplyE@. Has the same infxity and operator precedence as '$'
(!>) :: Encode w (a -> t) -> Encode ('Closed r) a -> Encode w t
x !> y = ApplyE x y

runE :: Encode w t -> t
runE (Sum cn _) = cn
runE (Rec cn) = cn
runE (ApplyE f x) = runE f (runE x)
runE (To x) = x
runE (Enc x) = x
runE (E _ x) = x
runE (MapE f x) = f $ runE x
runE (OmitC x) = x
runE (Omit _ x) = runE x
runE (Tag _ x) = runE x
runE (Key _ x) = runE x
runE (Keyed cn) = cn

gsize :: Encode w t -> Word
gsize (Sum _ _) = 0
gsize (Rec _) = 0
gsize (To _) = 1
gsize (Enc _) = 1
gsize (E _ _) = 1
gsize (MapE _ x) = gsize x
gsize (ApplyE f x) = gsize f + gsize x
gsize (OmitC _) = 0
gsize (Omit p x) = if p (runE x) then 0 else gsize x
gsize (Tag _ _) = 1
gsize (Key _ x) = gsize x
gsize (Keyed _) = 0

-- | Translate a first-order @(Encode w d) domain specific langage program, into an 'Encoding' .
encode :: Encode w t -> Encoding
encode = encodeCountPrefix 0
  where
    encodeCountPrefix :: Word -> Encode w t -> Encoding
    -- n is the number of fields we must write in the prefix.
    encodeCountPrefix n (Sum _ tag) = encodeListLen (n + 1) <> encodeWord tag
    encodeCountPrefix n (Keyed _) = encodeMapLen n
    encodeCountPrefix n (Rec _) = encodeListLen n
    encodeCountPrefix _ (To x) = toCBOR x
    encodeCountPrefix _ (Enc x) = fromPlainEncoding $ encCBOR x
    encodeCountPrefix _ (E enc x) = enc x
    encodeCountPrefix n (MapE _ x) = encodeCountPrefix n x
    encodeCountPrefix _ (OmitC _) = mempty
    encodeCountPrefix n (Tag tag x) = encodeTag tag <> encodeCountPrefix n x
    encodeCountPrefix n (Key tag x) = encodeWord tag <> encodeCountPrefix n x
    encodeCountPrefix n (Omit p x) =
      if p (runE x) then mempty else encodeCountPrefix n x
    encodeCountPrefix n (ApplyE ff xx) = encodeCountPrefix (n + gsize xx) ff <> encodeClosed xx
      where
        encodeClosed :: Encode ('Closed d) t -> Encoding
        encodeClosed (Rec _) = mempty
        encodeClosed (To x) = toCBOR x
        encodeClosed (Enc x) = fromPlainEncoding $ encCBOR x
        encodeClosed (E enc x) = enc x
        encodeClosed (MapE _ x) = encodeClosed x
        encodeClosed (ApplyE f x) = encodeClosed f <> encodeClosed x
        encodeClosed (OmitC _) = mempty
        encodeClosed (Omit p x) =
          if p (runE x) then mempty else encodeClosed x
        encodeClosed (Tag tag x) = encodeTag tag <> encodeClosed x
        encodeClosed (Key tag x) = encodeWord tag <> encodeClosed x
        encodeClosed (Keyed _) = mempty

encodeKeyedStrictMaybeWith ::
  Word ->
  (a -> Encoding) ->
  StrictMaybe a ->
  Encode ('Closed 'Sparse) (StrictMaybe a)
encodeKeyedStrictMaybeWith _ _ SNothing = OmitC SNothing
encodeKeyedStrictMaybeWith key enc (SJust x) = Key key (MapE SJust $ E enc x)

encodeKeyedStrictMaybe ::
  ToCBOR a =>
  Word ->
  StrictMaybe a ->
  Encode ('Closed 'Sparse) (StrictMaybe a)
encodeKeyedStrictMaybe key = encodeKeyedStrictMaybeWith key toCBOR

-- | Use `encodeDual` and `Cardano.Ledger.Binary.Coders.decodeDual`, when you want to
-- guarantee that a type has both `ToCBOR` and `FromCBR` instances.
encodeDual :: forall t. (ToCBOR t, FromCBOR t) => t -> Encode ('Closed 'Dense) t
encodeDual = E toCBOR
  where
    -- Enforce FromCBOR constraint on t
    _fromCBOR = fromCBOR :: Decoder s t
