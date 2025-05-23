{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Coders (
  -- * Creating decoders.

  --
  -- $Decoders
  Decode (..),
  (<!),
  (<*!),
  (<?),
  decode,
  decodeSparse,
  mapCoder,

  -- * Index types for well-formed Coders.

  --
  -- $Indexes
  Density (..),
  Wrapped (..),
  Field (..),
  ofield,
  invalidField,
  field,
  fieldGuarded,
  fieldA,
  fieldAA,

  -- * Using Duals
  decodeDual,

  -- * Containers, Combinators

  --
  -- $Combinators
  listDecodeA,
  mapDecodeA,
  setDecodeA,

  -- * Low level (Encoding/Decoder) utility functions
  decodeRecordNamed,
  decodeRecordNamedT,
  decodeRecordSum,
  invalidKey,
  unusedRequiredKeys,
  duplicateKey,
  guardUntilAtLeast,
) where

import Cardano.Ledger.Binary.Decoding.Annotated (Annotator (..), decodeAnnSet)
import Cardano.Ledger.Binary.Decoding.DecCBOR (DecCBOR (decCBOR))
import Cardano.Ledger.Binary.Decoding.Decoder
import Cardano.Ledger.Binary.Encoding.EncCBOR (EncCBOR (encCBOR))
import Cardano.Ledger.Binary.Group (DecCBORGroup (..), EncCBORGroup (..))
import Cardano.Ledger.Binary.Version (Version)
#if ! MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Set (Set, insert, member)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Typeable (Proxy (..), Typeable, typeOf)
import Data.Void (Void)

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
-- The advantage of using Encode with a MemoBytes, is we don't have to make a EncCBOR
-- instance. Instead the "instance" is spread amongst the pattern constuctors by using
-- (memoBytes encoding) in the where clause of the pattern contructor.
-- See some examples of this see the file Timelocks.hs
--

-- ========================================================
-- Subsidary classes and datatype used in the Coders scheme
-- =========================================================

-- $Indexes
--  Some CBOR instances wrap encoding sequences with prefixes and suffixes. I.e.
--  prefix , encode, encode, encode , ... , suffix.
--  There are two kinds of wrapping coders: Nary sums, and Sparsely encoded products.
--  Coders in these classes can only be decoded when they are wrapped by their
--  closing forms 'Summands' and 'SparseKeyed'. Another dimension, where we use indexes
--  to maintain type safety, are records which can be
--  encoded densely (all their fields serialised) or sparsely (only some of their
--  fields). We use indexes to types to try and mark (and enforce) these distinctions.

-- | Index for record density. Distinguishing (all the fields) from (some of the fields).
data Density = Dense | Sparse

-- | Index for a wrapped Coder. Wrapping is necessary for 'Summands' and 'SparseKeyed'.
data Wrapped where
  Open :: Wrapped -- Needs some type-wide wrapping
  Closed :: Density -> Wrapped -- Does not need type-wide wrapping,
  -- But may need field-wide wrapping, when Density is 'Sparse

-- | A Field pairs an update function and a decoder for one field of a Sparse record.
data Field t where
  Field :: (x -> t -> t) -> (forall s. Decoder s x) -> Field t

{-# INLINE field #-}
field :: Typeable x => (x -> t -> t) -> Decode ('Closed d) x -> Field t
field update dec = Field update (decode dec)

{-# INLINE fieldGuarded #-}
fieldGuarded ::
  Typeable x =>
  -- | The message to use if the condition fails
  String ->
  -- | The condition to guard against
  (x -> Bool) ->
  (x -> t -> t) ->
  Decode ('Closed d) x ->
  Field t
fieldGuarded failMsg check update dec =
  Field
    update
    ( decode dec >>= \x ->
        x <$ when (check x) (fail failMsg)
    )

{-# INLINE ofield #-}
ofield :: Typeable x => (StrictMaybe x -> t -> t) -> Decode ('Closed d) x -> Field t
ofield update dec = Field update (SJust <$> decode dec)

{-# INLINE invalidField #-}
invalidField :: forall t. Word -> Field t
invalidField n = field (flip $ const @t @Void) (Invalid n)

-- | Sparse decode something with a (DecCBOR (Annotator t)) instance
-- A special case of 'field'
fieldA :: (Typeable x, Applicative ann) => (x -> t -> t) -> Decode ('Closed d) x -> Field (ann t)
fieldA update dec = Field (liftA2 update) (pure <$> decode dec)
{-# INLINE fieldA #-}

-- | Sparse decode something with a (DecCBOR (Annotator t)) instance
fieldAA ::
  (Typeable x, Typeable ann, Applicative ann) =>
  (x -> t -> t) ->
  Decode ('Closed d) (ann x) ->
  Field (ann t)
fieldAA update dec = Field (liftA2 update) (decode dec)
{-# INLINE fieldAA #-}

-- ==================================================================
-- Decode
-- ===================================================================

-- | The type @('Decode' t)@ is designed to be dual to @('Encode' t)@. It was designed so that
-- in many cases a decoder can be extracted from an encoder by visual inspection. We now give some
-- example of @(Decode t)@  and  @(Encode t)@ pairs.
--
--
-- An example with 1 constructor (a record) uses 'Rec' and 'RecD'
--
-- In this example, let @Int@ and @C@ have 'EncCBOR' instances.
--
-- @
-- data C = C { unC :: Text.Text }
-- instance EncCBOR C where
--   encCBOR (C t) = encCBOR t
-- instance DecCBOR C where
--   decCBOR = C <$> decCBOR
--
-- data B = B { unB :: Text.Text }
--
-- data A = ACon Int B C
--
-- encodeA :: A -> Encode ('Closed 'Dense) A
-- encodeA (ACon i b c) = Rec ACon !> To i !> E (encCBOR . unB) b !> To c
--
-- decodeA :: Decode ('Closed 'Dense) A
-- decodeA = RecD ACon <! From <! D (B <$> decCBOR) <! From
--
-- instance EncCBOR A where
--   encCBOR = encode . encodeA
-- instance DecCBOR A where
--   decCBOR = decode decodeA
-- @
--
-- An example with multiple constructors uses 'Sum', 'SumD', and 'Summands'.
--
-- @
-- data N = N1 Int | N2 B Bool | N3 A
--
-- encodeN :: N -> Encode 'Open N
-- encodeN (N1 i)    = Sum N1 0 !> To i
-- encodeN (N2 b tf) = Sum N2 1 !> E (encCBOR . unB) b !> To tf
-- encodeN (N3 a)    = Sum N3 2 !> To a
--
-- decodeN :: Decode ('Closed 'Dense) N    -- Note each clause has an 'Open decoder,
-- decodeN = Summands "N" decodeNx           -- But Summands returns a ('Closed 'Dense) decoder
--   where decodeNx 0 = SumD N1 <! From
--         decodeNx 1 = SumD N2 <! D (B <$> decCBOR) <! From
--         decodeNx 3 = SumD N3 <! From
--         decodeNx k = Invalid k
--
-- instance EncCBOR N   where encCBOR x = encode(encodeN x)
-- instance DecCBOR N where decCBOR = decode decodeN
-- @
--
-- Two examples using variants of sparse encoding for records, i.e. those datatypes with only one constructor.
-- The Virtual constructor approach using 'Summands', 'OmitC', 'Emit'.
-- The Sparse field approach using 'Keyed', 'Key' and 'Omit'. The approaches work
-- because encoders and decoders don't put
-- fields with default values in the Encoding, and reconstruct the default values on the decoding side.
-- We will illustrate the two approaches using the datatype M
--
-- @
-- data M = M Int [Bool] Text.Text
--   deriving (Show, Typeable)
--
-- a0, a1, a2, a3 :: M  -- Some illustrative examples, using things that might be given default values.
-- a0 = M 0 [] "ABC"
-- a1 = M 0 [True] "ABC"
-- a2 = M 9 [] "ABC"
-- a3 = M 9 [False] "ABC"
-- @
--
-- The virtual constructor strategy pretends there are mutiple constructors
-- Even though there is only one. We use invariants about the data to avoid
-- encoding some of the values. Note the use of 'Sum' with virtual constructor tags 0,1,2,3
--
-- @
-- encM :: M -> Encode 'Open M
-- encM (M 0 [] t) = Sum M 0 !> OmitC 0 !> OmitC [] !> To t
-- encM (M 0 bs t) = Sum M 1 !> OmitC 0 !> To bs !> To t
-- encM (M n [] t) = Sum M 2 !> To n !> OmitC [] !> To t
-- encM (M n bs t) = Sum M 3 !> To n !> To bs !> To t
--
-- decM :: Word -> Decode 'Open M
-- decM 0 = SumD M <! Emit 0 <! Emit [] <! From  -- The virtual constructors tell which fields have been Omited
-- decM 1 = SumD M <! Emit 0 <! From <! From     -- So those fields are reconstructed using 'Emit'.
-- decM 2 = SumD M <! From <! Emit [] <! From
-- decM 3 = SumD M <! From <! From <! From
-- decM n = Invalid n
--
-- instance EncCBOR M where
--   encCBOR m = encode (encM m)
--
-- instance DecCBOR M where
--   decCBOR = decode (Summands "M" decM)
-- @
--
-- The Sparse field approach uses N keys, one for each field that is not defaulted. For example
-- @(M 9 [True] (pack "hi")))@. Here zero fields are defaulted, so there should be 3 keys.
-- Encoding this example would look something like this.
--
-- @
-- [TkMapLen 3,TkInt 0,TkInt 9,TkInt 1,TkListBegin,TkBool True,TkBreak,TkInt 2,TkString "hi"]
--                   ^key            ^key                                    ^key
-- @
--
-- So the user supplies a function, that encodes every field, each field must use a unique
-- key, and fields with default values have Omit wrapped around the Key encoding.
-- The user must ensure that there is NOT an Omit on a required field. 'encM2' is an example.
--
-- @
-- encM2:: M -> Encode ('Closed 'Sparse) M
-- encM2 (M n xs t) =
--     Keyed M
--        !> Omit (== 0) (Key 0 (To n))    -- Omit if n is zero
--        !> Omit null (Key 1 (To xs))     -- Omit if xs is null
--        !> Key 2 (To t)                  -- Always encode t
-- @
--
-- To write an Decoder we must pair a decoder for each field, with a function that updates only
-- that field. We use the 'Field' GADT to construct these pairs, and we must write a function, that
-- for each field tag, picks out the correct pair. If the Encode and Decode don't agree on how the
-- tags correspond to a particular field, things will fail.
--
-- @
-- boxM :: Word -> Field M
-- boxM 0 = field update0 From
--   where
--     update0 n (M _ xs t) = M n xs t
-- boxM 1 = field update1 From
--   where
--     update1 xs (M n _ t) = M n xs t
-- boxM 2 = field update2 From
--   where
--     update2 t (M n xs _) = M n xs t
-- boxM n = invalidField n
-- @
--
-- Finally there is a new constructor for 'Decode', called 'SparseKeyed', that decodes field
-- keyed sparse objects. The user supplies an initial value and field function, and a list
-- of tags of the required fields. The initial value should have default values and
-- any well type value in required fields. If the encode function (baz above) is
-- encoded properly the required fields in the initial value should always be over
-- overwritten. If it is not written properly, or a bad encoding comes from somewhere
-- else, the intial values in the required fields might survive decoding. The list
-- of required fields is checked.
--
-- @
-- instance DecCBOR M where
--   decCBOR = decode (SparseKeyed
--                       "TT"                        -- ^ Name of the type being decoded
--                       (M 0 [] (Text.pack "a"))  -- ^ The default value
--                       boxM                      -- ^ The Field function
--                       [(2, "Stringpart")]         -- ^ The required Fields
--                     )
--
-- instance EncCBOR M where
--   encCBOR m = encode(encM2 m)
-- @
data Decode (w :: Wrapped) t where
  -- | Label the constructor of a Record-like datatype (one with exactly 1 constructor) as a Decode.
  RecD :: t -> Decode ('Closed 'Dense) t
  -- | Label the constructor of a Record-like datatype (one with multiple constructors) as an Decode.
  SumD :: t -> Decode 'Open t
  -- | Lift a Word to Decode function into a DeCode for a type with multiple constructors.
  Summands :: Text.Text -> (Word -> Decode 'Open t) -> Decode ('Closed 'Dense) t
  -- | Lift a Word to Field function into a DeCode for a type with 1 constructor stored sparsely
  SparseKeyed ::
    Typeable t =>
    -- | Name of the Type (for error messages)
    String ->
    -- | The type with default values in all fields
    t ->
    -- | What to do with key in the @Word@
    (Word -> Field t) ->
    -- | Pairs of keys and Strings which must be there (default values not allowed)
    [(Word, String)] ->
    Decode ('Closed 'Dense) t
  -- | Label a (component, field, argument) as sparsely stored, which will be populated
  -- with the default value.
  KeyedD :: t -> Decode ('Closed 'Sparse) t
  -- | Label a (component, field, argument). It will be decoded using the existing
  -- DecCBOR instance at @t@
  From :: DecCBOR t => Decode w t
  -- | Label components, fields, arguments. It will be decoded using the existing
  -- DecCBORGroup instance at @t@
  FromGroup :: (EncCBORGroup t, DecCBORGroup t) => Decode w t
  -- | Label a (component, field, argument). It will be decoded using the given decoder.
  D :: (forall s. Decoder s t) -> Decode ('Closed 'Dense) t
  -- | Apply a functional decoding (arising from 'RecD' or 'SumD') to get (type wise)
  -- smaller decoding.
  ApplyD :: Typeable a => Decode w1 (a -> t) -> Decode ('Closed d) a -> Decode w1 t
  -- | Mark a Word as a Decoding which is not a valid Decoding. Used when decoding sums
  -- that are tagged out of range.
  Invalid :: Word -> Decode w t
  -- | Used to make (Decode w) an instance of Functor.
  Map :: Typeable a => (a -> b) -> Decode w a -> Decode w b
  -- | Assert that the next thing decoded must be tagged with the given word.
  TagD :: Word -> Decode ('Closed x) t -> Decode ('Closed x) t
  -- | Decode the next thing, not by inspecting the bytes, but pulled out of thin air,
  -- returning @t@. Used in sparse decoding.
  Emit :: t -> Decode w t
  -- The next two could be generalized to any (Applicative f) rather than Annotator

  -- | Lift a @(Decode w t)@ to a @(Decode w (Annotator t))@. Used on a (component, field,
  -- argument) that was not Annotator encoded, but contained in Record or Sum which is
  -- Annotator encoded.
  Ann :: Typeable t => Decode w t -> Decode w (Annotator t)
  -- | Apply a functional decoding (arising from 'RecD' or 'SumD' that needs to be
  -- Annotator decoded) to get (type wise) smaller decoding.
  ApplyAnn ::
    (Typeable a, Typeable t) =>
    -- | A functional Decode
    Decode w1 (Annotator (a -> t)) ->
    -- | An Decoder for an Annotator
    Decode ('Closed d) (Annotator a) ->
    Decode w1 (Annotator t)
  -- | the function to Either can raise an error when applied by returning (Left errorMessage)
  ApplyErr :: Typeable a => Decode w1 (a -> Either String t) -> Decode ('Closed d) a -> Decode w1 t

infixl 4 <!

infixl 4 <*!

infixl 4 <?

-- | Infix form of @ApplyD@ with the same infixity and precedence as @($)@.
(<!) :: Typeable a => Decode w1 (a -> t) -> Decode ('Closed w) a -> Decode w1 t
x <! y = ApplyD x y
{-# INLINE (<!) #-}

-- | Infix form of @ApplyAnn@ with the same infixity and precedence as @($)@.
(<*!) ::
  (Typeable a, Typeable t) =>
  Decode w1 (Annotator (a -> t)) -> Decode ('Closed d) (Annotator a) -> Decode w1 (Annotator t)
x <*! y = ApplyAnn x y
{-# INLINE (<*!) #-}

-- | Infix form of @ApplyErr@ with the same infixity and precedence as @($)@.
(<?) :: Typeable a => Decode w1 (a -> Either String t) -> Decode ('Closed d) a -> Decode w1 t
f <? y = ApplyErr f y
{-# INLINE (<?) #-}

hsize :: forall w t. Decode w t -> Int
hsize (Summands _ _) = 1
hsize (SumD _) = 0
hsize (RecD _) = 0
hsize (KeyedD _) = 0
hsize From = 1
hsize FromGroup = fromIntegral $ listLenBound $ Proxy @t
hsize (D _) = 1
hsize (ApplyD f x) = hsize f + hsize x
hsize (Invalid _) = 0
hsize (Map _ x) = hsize x
hsize (Emit _) = 0
hsize SparseKeyed {} = 1
hsize (TagD _ _) = 1
hsize (Ann x) = hsize x
hsize (ApplyAnn f x) = hsize f + hsize x
hsize (ApplyErr f x) = hsize f + hsize x
{-# INLINE hsize #-}

decode :: Typeable t => Decode w t -> Decoder s t
decode x = fmap snd (decodE x)
{-# INLINE decode #-}

decodE :: Typeable t => Decode w t -> Decoder s (Int, t)
decodE x = decodeCount x 0
{-# INLINE decodE #-}

decodeCount :: forall (w :: Wrapped) s t. Typeable t => Decode w t -> Int -> Decoder s (Int, t)
decodeCount (Summands nm f) n = (n + 1,) <$> decodeRecordSum nm (decodE . f)
decodeCount (SumD cn) n = pure (n + 1, cn)
decodeCount (KeyedD cn) n = pure (n + 1, cn)
decodeCount (RecD cn) n = decodeRecordNamed "RecD" (const n) (pure (n, cn))
decodeCount From n = (n,) <$> decCBOR
decodeCount FromGroup n = (n,) <$> decCBORGroup
decodeCount (D dec) n = (n,) <$> dec
decodeCount (Invalid k) _ = invalidKey k
decodeCount (Map f x) n = do (m, y) <- decodeCount x n; pure (m, f y)
decodeCount (Emit x) n = pure (n, x)
decodeCount (TagD expectedTag decoder) n = do
  assertTag expectedTag
  decodeCount decoder n
decodeCount (SparseKeyed name initial pick required) n =
  (n + 1,) <$> decodeSparse name initial pick required
decodeCount (Ann x) n = do (m, y) <- decodeCount x n; pure (m, pure y)
decodeCount (ApplyAnn g x) n = do
  (i, f) <- decodeCount g (n + hsize x)
  y <- decodeClosed x
  pure (i, f <*> y)
decodeCount (ApplyD cn g) n = do
  (i, f) <- decodeCount cn (n + hsize g)
  y <- decodeClosed g
  pure (i, f y)
decodeCount (ApplyErr cn g) n = do
  (i, f) <- decodeCount cn (n + hsize g)
  y <- decodeClosed g
  case f y of
    Right z -> pure (i, z)
    Left message -> cborError $ DecoderErrorCustom "decoding error:" (Text.pack message)
{-# INLINE decodeCount #-}

-- The type of DecodeClosed precludes pattern match against (SumD c) as the types are different.

decodeClosed :: Typeable t => Decode ('Closed d) t -> Decoder s t
decodeClosed (Summands nm f) = decodeRecordSum nm (decodE . f)
decodeClosed (KeyedD cn) = pure cn
decodeClosed (RecD cn) = pure cn
decodeClosed From = decCBOR
decodeClosed FromGroup = decCBORGroup
decodeClosed (D dec) = dec
decodeClosed (ApplyD cn g) = do
  f <- decodeClosed cn
  y <- decodeClosed g
  pure (f y)
decodeClosed (Invalid k) = invalidKey k
decodeClosed (Map f x) = f <$> decodeClosed x
decodeClosed (Emit n) = pure n
decodeClosed (TagD expectedTag decoder) = do
  assertTag expectedTag
  decodeClosed decoder
decodeClosed (SparseKeyed name initial pick required) =
  decodeSparse name initial pick required
decodeClosed (Ann x) = fmap pure (decodeClosed x)
decodeClosed (ApplyAnn g x) = do
  f <- decodeClosed g
  y <- decodeClosed x
  pure (f <*> y)
decodeClosed (ApplyErr cn g) = do
  f <- decodeClosed cn
  y <- decodeClosed g
  case f y of
    Right z -> pure z
    Left message -> cborError $ DecoderErrorCustom "decoding error:" (Text.pack message)
{-# INLINE decodeClosed #-}

decodeSparse ::
  Typeable a =>
  String ->
  a ->
  (Word -> Field a) ->
  [(Word, String)] ->
  Decoder s a
decodeSparse name initial pick required = do
  lenOrIndef <- decodeMapLenOrIndef
  (!v, used) <- case lenOrIndef of
    Just len -> getSparseBlock len initial pick Set.empty name
    Nothing -> getSparseBlockIndef initial pick Set.empty name
  if all (\(key, _name) -> member key used) required
    then pure v
    else unusedRequiredKeys used required (show (typeOf initial))
{-# INLINE decodeSparse #-}

-- | Given a function that picks a Field from a key, decodes that field
--   and returns a (t -> t) transformer, which when applied, will
--   update the record with the value decoded.
applyField :: (Word -> Field t) -> Set Word -> String -> Decoder s (t -> t, Set Word)
applyField f seen name = do
  tag <- decodeWord
  if Set.member tag seen
    then duplicateKey name tag
    else case f tag of
      Field update d -> d >>= \v -> pure (update v, insert tag seen)
{-# INLINE applyField #-}

-- | Decode a Map Block of key encoded data for type t
--   given a function that picks the right box for a given key, and an
--   initial value for the record (usually starts filled with default values).
--   The Block can be either len-encoded or block-encoded.
getSparseBlock :: Int -> t -> (Word -> Field t) -> Set Word -> String -> Decoder s (t, Set Word)
getSparseBlock 0 initial _pick seen _name = pure (initial, seen)
getSparseBlock n initial pick seen name = do
  (transform, seen2) <- applyField pick seen name
  getSparseBlock (n - 1) (transform initial) pick seen2 name
{-# INLINE getSparseBlock #-}

getSparseBlockIndef :: t -> (Word -> Field t) -> Set Word -> String -> Decoder s (t, Set Word)
getSparseBlockIndef initial pick seen name =
  decodeBreakOr >>= \case
    True -> pure (initial, seen)
    False -> do
      (transform, seen2) <- applyField pick seen name
      getSparseBlockIndef (transform initial) pick seen2 name
{-# INLINE getSparseBlockIndef #-}

mapCoder :: Typeable a => (a -> b) -> Decode w a -> Decode w b
mapCoder f (Map g x) = Map (f . g) x
mapCoder f x = Map f x
{-# INLINE mapCoder #-}

-- | Use `Cardano.Ledger.Binary.Coders.encodeDual` and `decodeDual`, when you want to
-- guarantee that a type has both `EncCBOR` and `FromCBR` instances.
decodeDual :: forall t. (EncCBOR t, DecCBOR t) => Decode ('Closed 'Dense) t
decodeDual = D decCBOR
  where
    -- Enforce EncCBOR constraint on t
    _encCBOR = encCBOR (undefined :: t)
{-# INLINE decodeDual #-}

-- =============================================================================

listDecodeA ::
  Typeable x => Decode ('Closed 'Dense) (Annotator x) -> Decode ('Closed 'Dense) (Annotator [x])
listDecodeA dx = D (sequence <$> decodeList (decode dx))
{-# INLINE listDecodeA #-}

setDecodeA ::
  (Ord x, Typeable x) =>
  Decode ('Closed 'Dense) (Annotator x) ->
  Decode ('Closed 'Dense) (Annotator (Set x))
setDecodeA dx = D (decodeAnnSet (decode dx))
{-# INLINE setDecodeA #-}

mapDecodeA ::
  (Ord k, Typeable k, Typeable v) =>
  Decode ('Closed 'Dense) (Annotator k) ->
  Decode ('Closed 'Dense) (Annotator v) ->
  Decode ('Closed 'Dense) (Annotator (Map.Map k v))
mapDecodeA k v = D (decodeMapTraverse (decode k) (decode v))
{-# INLINE mapDecodeA #-}

--------------------------------------------------------------------------------
-- Utility functions for working with CBOR
--------------------------------------------------------------------------------

duplicateKey :: String -> Word -> Decoder s a
duplicateKey name k =
  cborError $
    DecoderErrorCustom
      "Duplicate key:"
      (Text.pack $ show k ++ " while decoding type " ++ name)
{-# NOINLINE duplicateKey #-}

unusedRequiredKeys :: Set Word -> [(Word, String)] -> String -> Decoder s a
unusedRequiredKeys used required name =
  cborError $
    DecoderErrorCustom
      (Text.pack ("value of type " ++ name))
      (Text.pack (message (filter bad required)))
  where
    bad (k, _) = not (member k used)
    message [] = ", not decoded."
    message [pair] = report pair ++ message []
    message (pair : more) = report pair ++ ", and " ++ message more
    report (k, f) = "field " ++ f ++ " with key " ++ show k
{-# NOINLINE unusedRequiredKeys #-}

-- | Prevent decoding until the 'Version' is at least the provided version.
guardUntilAtLeast :: DecCBOR a => String -> Version -> Decode ('Closed 'Dense) a
guardUntilAtLeast errMessage v = D (unlessDecoderVersionAtLeast v (fail errMessage) >> decCBOR)
{-# INLINE guardUntilAtLeast #-}
