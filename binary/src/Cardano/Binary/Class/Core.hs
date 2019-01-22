{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NumDecimals               #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

-- | The 'Bi' typeclass, instances, and primitive functions
--
--   The 'Bi' typeclass is a wrapper around the `cborg` library providing:
--
--   > encode :: a -> Encoding
--   > decode :: Decoder s a
--
--   as well as symbolic 'Size' estimates for the resulting encoding.
--
--   We encode product types as known-length CBOR lists and sum types as
--   known-length lists with an extra @tag :: Word8@ as the first element.

module Cardano.Binary.Class.Core
  ( Bi(..)
  , enforceSize
  , matchSize
  , DecoderError(..)

    -- * CBOR re-exports
  , E.encodeListLen
  , D.decodeListLen
  , D.decodeListLenOf
  , E.Encoding
  , D.Decoder
  , CBOR.Read.deserialiseIncremental
  , CBOR.Write.toLazyByteString
  , CBOR.Write.toBuilder
  , CBOR.Read.IDecode(..)

    -- * Utils
  , withWordSize
  , decodeListWith
  , decodeMaybe

    -- * Size of expressions
  , Range(..)
  , szEval
  , Size
  , Case(..)
  , caseValue
  , LengthOf(..)
  , SizeOverride(..)
  , isTodo
  , szCases
  , szLazy
  , szGreedy
  , szForce
  , szWithCtx
  , szSimplify
  , apMono
  , szBounds
  )
where

import Cardano.Prelude

import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Char as Char
import Data.Fixed (E12, Fixed(..), Nano, Pico, resolution)
import Data.Functor.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tagged (Tagged(..))
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (TypeRep, typeRep)
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic as Vector.Generic
import Foreign.Storable (sizeOf)
import Formatting (bprint, build, int, shown, stext)
import qualified Formatting.Buildable as B (Buildable(..))


--------------------------------------------------------------------------------
-- DecoderError
--------------------------------------------------------------------------------

data DecoderError
  = DecoderErrorCanonicityViolation Text
  | DecoderErrorCustom Text Text
  -- ^ Custom decoding error, usually due to some validation failure
  | DecoderErrorDeserialiseFailure Text CBOR.Read.DeserialiseFailure
  | DecoderErrorEmptyList Text
  | DecoderErrorLeftover Text BS.ByteString
  | DecoderErrorSizeMismatch Text Int Int
  -- ^ A size mismatch @DecoderErrorSizeMismatch label expectedSize actualSize@
  | DecoderErrorUnknownTag Text Word8
  | DecoderErrorVoid
  deriving (Eq, Show)

instance B.Buildable DecoderError where
  build = \case
    DecoderErrorCanonicityViolation lbl ->
      bprint ("Canonicity violation while decoding " . stext) lbl

    DecoderErrorCustom lbl err -> bprint
      ("An error occured while decoding " . stext . ".\n"
      . "Error: " . stext)
      lbl
      err

    DecoderErrorDeserialiseFailure lbl failure -> bprint
      ( "Deserialisation failure while decoding " . stext . ".\n"
      . "CBOR failed with error: " . shown
      )
      lbl
      failure

    DecoderErrorEmptyList lbl ->
      bprint ("Found unexpected empty list while decoding " . stext) lbl

    DecoderErrorLeftover lbl leftover -> bprint
      ( "Found unexpected leftover bytes while decoding " . stext . "./n"
      . "Leftover: " . shown
      )
      lbl
      leftover

    DecoderErrorSizeMismatch lbl requested actual -> bprint
      ( "Size mismatch when decoding " . stext . ".\n"
      . "Expected " . int . ", but found " . int . "."
      )
      lbl
      requested
      actual

    DecoderErrorUnknownTag lbl t ->
      bprint ("Found unknown tag " . int . " while decoding " . stext) t lbl

    DecoderErrorVoid -> bprint "Attempted to decode Void"


--------------------------------------------------------------------------------
-- Useful primitives
--------------------------------------------------------------------------------

-- | Enforces that the input size is the same as the decoded one, failing in
--   case it's not
enforceSize :: Text -> Int -> D.Decoder s ()
enforceSize lbl requestedSize = D.decodeListLen >>= matchSize lbl requestedSize

-- | Compare two sizes, failing if they are not equal
matchSize :: Text -> Int -> Int -> D.Decoder s ()
matchSize lbl requestedSize actualSize =
  when (actualSize /= requestedSize) $ cborError $ DecoderErrorSizeMismatch
    lbl
    requestedSize
    actualSize

-- | Compute encoded size of an integer
withWordSize :: (Integral s, Integral a) => s -> a
withWordSize x =
  let s = fromIntegral x :: Integer
  in
    if
      | s <= 0x17 && s >= (-0x18) -> 1
      | s <= 0xff && s >= (-0x100) -> 2
      | s <= 0xffff && s >= (-0x10000) -> 3
      | s <= 0xffffffff && s >= (-0x100000000) -> 5
      | otherwise -> 9


--------------------------------------------------------------------------------
-- Bi
--------------------------------------------------------------------------------

class Typeable a => Bi a where
  encode :: a -> E.Encoding
  decode :: D.Decoder s a

  label :: Proxy a -> Text
  label = show . typeRep

  encodeList :: [a] -> E.Encoding
  encodeList = defaultEncodeList

  decodeList :: D.Decoder s [a]
  decodeList = defaultDecodeList

  -- | Generate a symbolic expression representing the size bounds for
  --   encoded values of this type. If the size depends on sizes of other types,
  --   the first argument should be used to compute those sizes. This allows
  --   the user to select between different evaluation strategies and to override
  --   the size of specific types.
  encodedSizeExpr :: (forall t. Bi t => Proxy t -> Size) -> Proxy a -> Size
  encodedSizeExpr = todo

  encodedListSizeExpr :: (forall t. Bi t => Proxy t -> Size) -> Proxy [a] -> Size
  encodedListSizeExpr = defaultEncodedListSizeExpr

-- | Default @'E.Encoding'@ for list types.
defaultEncodeList :: Bi a => [a] -> E.Encoding
defaultEncodeList xs =
  E.encodeListLenIndef <> foldr (\x r -> encode x <> r) E.encodeBreak xs

-- | Default @'D.Decoder'@ for list types.
defaultDecodeList :: Bi a => D.Decoder s [a]
defaultDecodeList = decodeListWith decode

-- | @'D.Decoder'@ for list.
decodeListWith :: D.Decoder s a -> D.Decoder s [a]
decodeListWith d = do
  D.decodeListLenIndef
  D.decodeSequenceLenIndef (flip (:)) [] reverse d

-- | A type used to represent the length of a value in 'Size' computations.
newtype LengthOf xs = LengthOf xs

instance Typeable xs => Bi (LengthOf xs) where
  encode = panic "The `LengthOf` type cannot be encoded!"
  decode = panic "The `LengthOf` type cannot be decoded!"

-- | Default size expression for a list type.
defaultEncodedListSizeExpr
  :: forall a
   . Bi a
  => (forall t . Bi t => Proxy t -> Size)
  -> Proxy [a]
  -> Size
defaultEncodedListSizeExpr size _ =
  2 + size (Proxy @(LengthOf [a])) * size (Proxy @a)


--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance Bi () where
  encode = const E.encodeNull
  decode = D.decodeNull
  encodedSizeExpr _ _ = 1

instance Bi Bool where
  encode = E.encodeBool
  decode = D.decodeBool
  encodedSizeExpr _ _ = 1

instance Bi Char where
  encode c = E.encodeString (Text.singleton c)
  decode = do
    t <- D.decodeString
    matchSize "Char" 1 (Text.length t)
    pure $ Text.head t

  -- For [Char]/String we have a special encoding
  encodeList cs = E.encodeString (toS cs)
  decodeList = toS <$> D.decodeString

  encodedSizeExpr _ pxy = encodedSizeRange (Char.ord <$> pxy)
  encodedListSizeExpr size _ =
    let
      bsLength = size (Proxy @(LengthOf [Char]))
        * szCases [Case "minChar" 1, Case "maxChar" 4]
    in bsLength + apMono "withWordSize" withWordSize bsLength


--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance Bi Integer where
  encode = E.encodeInteger
  decode = D.decodeInteger

encodedSizeRange :: forall a . (Integral a, Bounded a) => Proxy a -> Size
encodedSizeRange _ = szCases
  [ mkCase "minBound" 0 -- min, in absolute value
  , mkCase "maxBound" maxBound
  ]
  where mkCase n x = Case n (fromIntegral $ (withWordSize :: a -> Integer) x)

instance Bi Word where
  encode = E.encodeWord
  decode = D.decodeWord
  encodedSizeExpr _ = encodedSizeRange

instance Bi Word8 where
  encode = E.encodeWord8
  decode = D.decodeWord8
  encodedSizeExpr _ = encodedSizeRange

instance Bi Word16 where
  encode = E.encodeWord16
  decode = D.decodeWord16
  encodedSizeExpr _ = encodedSizeRange

instance Bi Word32 where
  encode = E.encodeWord32
  decode = D.decodeWord32
  encodedSizeExpr _ = encodedSizeRange

instance Bi Word64 where
  encode = E.encodeWord64
  decode = D.decodeWord64
  encodedSizeExpr _ = encodedSizeRange

instance Bi Int where
  encode = E.encodeInt
  decode = D.decodeInt
  encodedSizeExpr _ = encodedSizeRange

instance Bi Float where
  encode = E.encodeFloat
  decode = D.decodeFloat
  encodedSizeExpr _ _ = 1 + fromIntegral (sizeOf (0 :: Float))

instance Bi Int32 where
  encode = E.encodeInt32
  decode = D.decodeInt32
  encodedSizeExpr _ = encodedSizeRange

instance Bi Int64 where
  encode = E.encodeInt64
  decode = D.decodeInt64
  encodedSizeExpr _ = encodedSizeRange

instance Bi Nano where
  encode (MkFixed nanoseconds) = encode nanoseconds
  decode = MkFixed <$> decode

instance Bi Pico where
  encode (MkFixed picoseconds) = encode picoseconds
  decode = MkFixed <$> decode

-- | For backwards compatibility we round pico precision to micro
instance Bi NominalDiffTime where
  encode = encode . (`div` 1e6) . toPicoseconds
   where
    toPicoseconds :: NominalDiffTime -> Integer
    toPicoseconds t =
      numerator (toRational t * toRational (resolution $ Proxy @E12))
  decode = fromRational . (% 1e6) <$> decode

instance Bi Natural where
  encode = encode . toInteger
  decode = fromInteger <$> decode

instance Bi Void where
  decode = cborError DecoderErrorVoid
  encode = absurd


--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, Bi a) => Bi (Tagged s a) where
  encode (Tagged a) = encode a
  decode = Tagged <$> decode
  encodedSizeExpr size _ = encodedSizeExpr size (Proxy @a)


--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a,b) where
  encode (a, b) = E.encodeListLen 2 <> encode a <> encode b
  decode = do
    D.decodeListLenOf 2
    !x <- decode
    !y <- decode
    return (x, y)
  encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @b)

instance (Bi a, Bi b, Bi c) => Bi (a,b,c) where
  encode (a, b, c) = E.encodeListLen 3 <> encode a <> encode b <> encode c

  decode = do
    D.decodeListLenOf 3
    !x <- decode
    !y <- decode
    !z <- decode
    return (x, y, z)

  encodedSizeExpr size _ =
    1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c)

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a,b,c,d) where
  encode (a, b, c, d) =
    E.encodeListLen 4 <> encode a <> encode b <> encode c <> encode d

  decode = do
    D.decodeListLenOf 4
    !a <- decode
    !b <- decode
    !c <- decode
    !d <- decode
    return (a, b, c, d)

  encodedSizeExpr size _ =
    1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c) + size (Proxy @d)

instance Bi BS.ByteString where
  encode = E.encodeBytes
  decode = D.decodeBytes
  encodedSizeExpr size _ =
    let len = size (Proxy @(LengthOf BS.ByteString))
    in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance Bi Text.Text where
  encode = E.encodeString
  decode = D.decodeString
  encodedSizeExpr size _ = encodedSizeExpr size (Proxy @[Char])

instance Bi BS.Lazy.ByteString where
  encode = encode . BS.Lazy.toStrict
  decode = BS.Lazy.fromStrict <$> decode
  encodedSizeExpr size _ =
    let len = size (Proxy @(LengthOf BS.Lazy.ByteString))
    in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance Bi a => Bi [a] where
  encode = encodeList
  decode = decodeList
  encodedSizeExpr size _ = encodedListSizeExpr size (Proxy @[a])

instance (Bi a, Bi b) => Bi (Either a b) where
  encode (Left  x) = E.encodeListLen 2 <> E.encodeWord 0 <> encode x
  encode (Right x) = E.encodeListLen 2 <> E.encodeWord 1 <> encode x

  decode = do
    D.decodeListLenOf 2
    t <- D.decodeWord
    case t of
      0 -> do
        !x <- decode
        return (Left x)
      1 -> do
        !x <- decode
        return (Right x)
      _ -> cborError $ DecoderErrorUnknownTag "Either" (fromIntegral t)

  encodedSizeExpr size _ = szCases
    [Case "Left" (2 + size (Proxy @a)), Case "Right" (2 + size (Proxy @b))]

instance Bi a => Bi (NonEmpty a) where
  encode = defaultEncodeList . toList
  decode = nonEmpty <$> defaultDecodeList >>= toCborError . \case
    Nothing -> Left $ DecoderErrorEmptyList "NonEmpty"
    Just xs -> Right xs
  encodedSizeExpr size _ = size (Proxy @[a]) -- MN TODO make 0 count impossible

instance Bi a => Bi (Maybe a) where
  encode Nothing  = E.encodeListLen 0
  encode (Just x) = E.encodeListLen 1 <> encode x

  decode = decodeMaybe decode

  encodedSizeExpr size _ =
    szCases [Case "Nothing" 1, Case "Just" (1 + size (Proxy @a))]


decodeMaybe :: D.Decoder s a -> D.Decoder s (Maybe a)
decodeMaybe decoder = do
  n <- D.decodeListLen
  case n of
    0 -> return Nothing
    1 -> do
      !x <- decoder
      return (Just x)
    _ -> cborError $ DecoderErrorUnknownTag "Maybe" (fromIntegral n)


encodeContainerSkel
  :: (Word -> E.Encoding)
  -> (container -> Int)
  -> (accumFunc -> E.Encoding -> container -> E.Encoding)
  -> accumFunc
  -> container
  -> E.Encoding
encodeContainerSkel encodeLen size foldFunction f c =
  encodeLen (fromIntegral (size c)) <> foldFunction f mempty c
{-# INLINE encodeContainerSkel #-}

decodeContainerSkelWithReplicate
  :: Bi a
  => D.Decoder s Int
  -- ^ How to get the size of the container
  -> (Int -> D.Decoder s a -> D.Decoder s container)
  -- ^ replicateM for the container
  -> ([container] -> container)
  -- ^ concat for the container
  -> D.Decoder s container
decodeContainerSkelWithReplicate decodeLen replicateFun fromList = do
  -- Look at how much data we have at the moment and use it as the limit for
  -- the size of a single call to replicateFun. We don't want to use
  -- replicateFun directly on the result of decodeLen since this might lead to
  -- DOS attack (attacker providing a huge value for length). So if it's above
  -- our limit, we'll do manual chunking and then combine the containers into
  -- one.
  size  <- decodeLen
  limit <- D.peekAvailable
  if size <= limit
    then replicateFun size decode
    else do
        -- Take the max of limit and a fixed chunk size (note: limit can be
        -- 0). This basically means that the attacker can make us allocate a
        -- container of size 128 even though there's no actual input.
      let
        chunkSize = max limit 128
        (d, m)    = size `divMod` chunkSize
        buildOne s = replicateFun s decode
      containers <- sequence $ buildOne m : replicate d (buildOne chunkSize)
      return $! fromList containers
{-# INLINE decodeContainerSkelWithReplicate #-}

encodeMapSkel
  :: (Bi k, Bi v)
  => (m -> Int)
  -> ((k -> v -> E.Encoding -> E.Encoding) -> E.Encoding -> m -> E.Encoding)
  -> m
  -> E.Encoding
encodeMapSkel size foldrWithKey = encodeContainerSkel
  E.encodeMapLen
  size
  foldrWithKey
  (\k v b -> encode k <> encode v <> b)
{-# INLINE encodeMapSkel #-}

-- | Checks canonicity by comparing the new key being decoded with
--   the previous one, to enfore these are sorted the correct way.
--   See: https://tools.ietf.org/html/rfc7049#section-3.9
--   "[..]The keys in every map must be sorted lowest value to highest.[...]"
decodeMapSkel :: (Ord k, Bi k, Bi v) => ([(k, v)] -> m) -> D.Decoder s m
decodeMapSkel fromDistinctAscList = do
  n <- D.decodeMapLen
  case n of
    0 -> return (fromDistinctAscList [])
    _ -> do
      (firstKey, firstValue) <- decodeEntry
      fromDistinctAscList
        <$> decodeEntries (n - 1) firstKey [(firstKey, firstValue)]
 where
    -- Decode a single (k,v).
  decodeEntry :: (Bi k, Bi v) => D.Decoder s (k, v)
  decodeEntry = do
    !k <- decode
    !v <- decode
    return (k, v)

  -- Decode all the entries, enforcing canonicity by ensuring that the
  -- previous key is smaller than the next one.
  decodeEntries
    :: (Bi k, Bi v, Ord k) => Int -> k -> [(k, v)] -> D.Decoder s [(k, v)]
  decodeEntries 0 _           acc  = pure $ reverse acc
  decodeEntries !remainingPairs previousKey !acc = do
    p@(newKey, _) <- decodeEntry
    -- Order of keys needs to be strictly increasing, because otherwise it's
    -- possible to supply lists with various amount of duplicate keys which
    -- will result in the same map as long as the last value of the given
    -- key on the list is the same in all of them.
    if newKey > previousKey
      then decodeEntries (remainingPairs - 1) newKey (p : acc)
      else cborError $ DecoderErrorCanonicityViolation "Map"
{-# INLINE decodeMapSkel #-}

instance (Ord k, Bi k, Bi v) => Bi (Map k v) where
  encode = encodeMapSkel M.size M.foldrWithKey
  decode = decodeMapSkel M.fromDistinctAscList

encodeSetSkel
  :: Bi a
  => (s -> Int)
  -> ((a -> E.Encoding -> E.Encoding) -> E.Encoding -> s -> E.Encoding)
  -> s
  -> E.Encoding
encodeSetSkel size foldFunction = mappend encodeSetTag . encodeContainerSkel
  E.encodeListLen
  size
  foldFunction
  (\a b -> encode a <> b)
{-# INLINE encodeSetSkel #-}

-- We stitch a `258` in from of a (Hash)Set, so that tools which
-- programmatically check for canonicity can recognise it from a normal
-- array. Why 258? This will be formalised pretty soon, but IANA allocated
-- 256...18446744073709551615 to "First come, first served":
-- https://www.iana.org/assignments/cbor-tags/cbor-tags.xhtml Currently `258` is
-- the first unassigned tag and as it requires 2 bytes to be encoded, it sounds
-- like the best fit.
setTag :: Word
setTag = 258

encodeSetTag :: E.Encoding
encodeSetTag = E.encodeTag setTag

decodeSetTag :: D.Decoder s ()
decodeSetTag = do
  t <- D.decodeTag
  when (t /= setTag) $ cborError $ DecoderErrorUnknownTag "Set" (fromIntegral t)

decodeSetSkel :: (Ord a, Bi a) => ([a] -> c) -> D.Decoder s c
decodeSetSkel fromDistinctAscList = do
  decodeSetTag
  n <- D.decodeListLen
  case n of
    0 -> return (fromDistinctAscList [])
    _ -> do
      firstValue <- decode
      fromDistinctAscList <$> decodeEntries (n - 1) firstValue [firstValue]
 where
  decodeEntries :: (Bi v, Ord v) => Int -> v -> [v] -> D.Decoder s [v]
  decodeEntries 0 _ acc  = pure $ reverse acc
  decodeEntries !remainingEntries previousValue !acc = do
    newValue <- decode
    -- Order of values needs to be strictly increasing, because otherwise
    -- it's possible to supply lists with various amount of duplicates which
    -- will result in the same set.
    if newValue > previousValue
      then decodeEntries (remainingEntries - 1) newValue (newValue : acc)
      else cborError $ DecoderErrorCanonicityViolation "Set"
{-# INLINE decodeSetSkel #-}

instance (Ord a, Bi a) => Bi (Set a) where
  encode = encodeSetSkel S.size S.foldr
  decode = decodeSetSkel S.fromDistinctAscList

-- | Generic encoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
encodeVector :: (Bi a, Vector.Generic.Vector v a) => v a -> E.Encoding
encodeVector = encodeContainerSkel
  E.encodeListLen
  Vector.Generic.length
  Vector.Generic.foldr
  (\a b -> encode a <> b)
{-# INLINE encodeVector #-}

-- | Generic decoder for vectors. Its intended use is to allow easy
-- definition of 'Serialise' instances for custom vector
decodeVector :: (Bi a, Vector.Generic.Vector v a) => D.Decoder s (v a)
decodeVector = decodeContainerSkelWithReplicate
  D.decodeListLen
  Vector.Generic.replicateM
  Vector.Generic.concat
{-# INLINE decodeVector #-}

instance (Bi a) => Bi (Vector.Vector a) where
  encode = encodeVector
  {-# INLINE encode #-}
  decode = decodeVector
  {-# INLINE decode #-}
  encodedSizeExpr size _ =
    2 + size (Proxy @(LengthOf (Vector.Vector a))) * size (Proxy @a)


--------------------------------------------------------------------------------
-- Size expressions
--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f .: g = \x y -> f (g x y)

-- | Expressions describing the statically-computed size bounds on
--   a type's possible values.
type Size = Fix SizeF

-- | The base functor for @Size@ expressions.
data SizeF t
  = AddF t t
  -- ^ Sum of two sizes.
  | MulF t t
  -- ^ Product of two sizes.
  | SubF t t
  -- ^ Difference of two sizes.
  | AbsF t
  -- ^ Absolute value of a size.
  | NegF t
  -- ^ Negation of a size.
  | SgnF t
  -- ^ Signum of a size.
  | CasesF [Case t]
  -- ^ Case-selection for sizes. Used for sum types.
  | ValueF Natural
  -- ^ A constant value.
  | ApF Text (Natural -> Natural) t
  -- ^ Application of a monotonic function to a size.
  | forall a. Bi a => TodoF (forall x. Bi x => Proxy x -> Size) (Proxy a)
  -- ^ A suspended size calculation ("thunk"). This is used to delay the
  --   computation of a size until some later point, which is useful for
  --   progressively building more detailed size estimates for a type
  --   from the outside in. For example, `szLazy` can be followed by
  --   applications of `szForce` to reveal more detailed expressions
  --   describing the size bounds on a type.

instance Functor SizeF where
  fmap f = \case
    AddF x y  -> AddF (f x) (f y)
    MulF x y  -> MulF (f x) (f y)
    SubF x y  -> SubF (f x) (f y)
    AbsF   x  -> AbsF (f x)
    NegF   x  -> NegF (f x)
    SgnF   x  -> SgnF (f x)
    CasesF xs -> CasesF (map (fmap f) xs)
    ValueF x  -> ValueF x
    ApF n g x -> ApF n g (f x)
    TodoF g x -> TodoF g x

instance Num (Fix SizeF) where
  (+) = Fix .: AddF
  (*) = Fix .: MulF
  (-) = Fix .: SubF
  negate = Fix . NegF
  abs = Fix . AbsF
  signum = Fix . SgnF
  fromInteger = Fix . ValueF . fromInteger

instance B.Buildable t => B.Buildable (SizeF t) where
  build x_
    = let
        showp2 :: (B.Buildable a, B.Buildable b) => a -> Text -> b -> Builder
        showp2 = bprint ("(" . build . " " . stext . " " . build . ")")
      in
        case x_ of
          AddF x y -> showp2 x "+" y
          MulF x y -> showp2 x "*" y
          SubF x y -> showp2 x "-" y
          NegF x   -> bprint ("-" . build) x
          AbsF x   -> bprint ("|" . build . "|") x
          SgnF x   -> bprint ("sgn(" . build . ")") x
          CasesF xs ->
            bprint ("{ " . build . "}") $ foldMap (bprint (build . " ")) xs
          ValueF x  -> bprint shown (toInteger x)
          ApF n _ x -> bprint (stext . "(" . build . ")") n x
          TodoF _ x -> bprint ("(_ :: " . shown . ")") (typeRep x)

instance B.Buildable (Fix SizeF) where
  build x = bprint build (unfix x)

-- | Create a case expression from individual cases.
szCases :: [Case Size] -> Size
szCases = Fix . CasesF

-- | An individual labeled case.
data Case t =
  Case Text t
  deriving (Functor)

-- | Discard the label on a case.
caseValue :: Case t -> t
caseValue (Case _ x) = x

instance B.Buildable t => B.Buildable (Case t) where
  build (Case n x) = bprint (stext . "=" . build) n x

-- | A range of values. Should satisfy the invariant @forall x. lo x <= hi x@.
data Range b = Range
  { lo :: b
  , hi :: b
  }

-- | The @Num@ instance for @Range@ uses interval arithmetic. Note that the
--   @signum@ method is not lawful: if the interval @x@ includes 0 in its
--   interior but is not symmetric about 0, then @abs x * signum x /= x@.
instance (Ord b, Num b) => Num (Range b) where
  x + y = Range {lo = lo x + lo y, hi = hi x + hi y}
  x * y =
    let products = [ u * v | u <- [lo x, hi x], v <- [lo y, hi y] ]
    in Range {lo = minimum products, hi = maximum products}
  x - y = Range {lo = lo x - hi y, hi = hi x - lo y}
  negate x = Range {lo = negate (hi x), hi = negate (lo x)}
  abs x = if
    | lo x <= 0 && hi x >= 0 -> Range {lo = 0, hi = max (hi x) (negate $ lo x)}
    | lo x <= 0 && hi x <= 0 -> Range {lo = negate (hi x), hi = negate (lo x)}
    | otherwise              -> x
  signum x = Range {lo = signum (lo x), hi = signum (hi x)}
  fromInteger n = Range {lo = fromInteger n, hi = fromInteger n}

instance B.Buildable (Range Natural) where
  build r = bprint (shown . ".." . shown) (toInteger $ lo r) (toInteger $ hi r)

-- | Fully evaluate a size expression by applying the given function to any
--   suspended computations. @szEval g@ effectively turns each "thunk"
--   of the form @TodoF f x@ into @g x@, then evaluates the result.
szEval
  :: (forall t . Bi t => (Proxy t -> Size) -> Proxy t -> Range Natural)
  -> Size
  -> Range Natural
szEval doit = cata $ \case
  AddF x y  -> x + y
  MulF x y  -> x * y
  SubF x y  -> x - y
  NegF   x  -> negate x
  AbsF   x  -> abs x
  SgnF   x  -> signum x
  CasesF xs -> Range
    { lo = minimum (map (lo . caseValue) xs)
    , hi = maximum (map (hi . caseValue) xs)
    }
  ValueF x  -> Range {lo = x, hi = x}
  ApF _ f x -> Range {lo = f (lo x), hi = f (hi x)}
  TodoF f x -> doit f x

{-| Evaluate the expression lazily, by immediately creating a thunk
    that will evaluate its contents lazily.

> ghci> putStrLn $ pretty $ szLazy (Proxy @TxAux)
> (_ :: TxAux)
-}
szLazy :: Bi a => (Proxy a -> Size)
szLazy = todo (encodedSizeExpr szLazy)

{-| Evaluate an expression greedily. There may still be thunks in the
    result, for types that did not provide a custom 'encodedSizeExpr' method
    in their 'Bi' instance.

> ghci> putStrLn $ pretty $ szGreedy (Proxy @TxAux)
> (0 + { TxAux=(2 + ((0 + (((1 + (2 + ((_ :: LengthOf [TxIn]) * (2 + { TxInUtxo=(2 + ((1 + 34) + { minBound=1 maxBound=5 })) })))) + (2 + ((_ :: LengthOf [TxOut]) * (0 + { TxOut=(2 + ((0 + ((2 + ((2 + withWordSize((((1 + 30) + (_ :: Attributes AddrAttributes)) + 1))) + (((1 + 30) + (_ :: Attributes AddrAttributes)) + 1))) + { minBound=1 maxBound=5 })) + { minBound=1 maxBound=9 })) })))) + (_ :: Attributes ()))) + (_ :: Vector TxInWitness))) })

-}
szGreedy :: Bi a => (Proxy a -> Size)
szGreedy = encodedSizeExpr szGreedy

-- | Is this expression a thunk?
isTodo :: Size -> Bool
isTodo (Fix (TodoF _ _)) = True
isTodo _ = False

-- | Create a "thunk" that will apply @f@ to @pxy@ when forced.
todo
  :: forall a . Bi a => (forall t . Bi t => Proxy t -> Size) -> Proxy a -> Size
todo f pxy = Fix (TodoF f pxy)

-- | Apply a monotonically increasing function to the expression.
--   There are three cases when applying @f@ to a @Size@ expression:
--      * When applied to a value @x@, compute @f x@.
--      * When applied to cases, apply to each case individually.
--      * In all other cases, create a deferred application of @f@.
apMono :: Text -> (Natural -> Natural) -> Size -> Size
apMono n f = \case
  Fix (ValueF x) -> Fix (ValueF (f x))
  Fix (CasesF cs) -> Fix (CasesF (map (fmap (apMono n f)) cs))
  x -> Fix (ApF n f x)

-- | Greedily compute the size bounds for a type, using the given context to
--   override sizes for specific types.
szWithCtx :: Bi a => Map TypeRep SizeOverride -> Proxy a -> Size
szWithCtx ctx pxy = case M.lookup (typeRep pxy) ctx of
  Nothing       -> normal
  Just override -> case override of
    SizeConstant   sz    -> sz
    SizeExpression f     -> f (szWithCtx ctx)
    SelectCases    names -> cata (selectCase names) normal
 where
  -- The non-override case
  normal = encodedSizeExpr (szWithCtx ctx) pxy

  selectCase :: [Text] -> SizeF Size -> Size
  selectCase names orig = case orig of
    CasesF cs -> matchCase names cs (Fix orig)
    _         -> Fix orig

  matchCase :: [Text] -> [Case Size] -> Size -> Size
  matchCase names cs orig =
    case filter (\(Case name _) -> name `elem` names) cs of
      []         -> orig
      [Case _ x] -> x
      cs'        -> Fix (CasesF cs')

-- | Override mechanisms to be used with 'szWithCtx'.
data SizeOverride
  = SizeConstant Size
  -- ^ Replace with a fixed @Size@.
  | SizeExpression ((forall a. Bi a => Proxy a -> Size) -> Size)
  -- ^ Recursively compute the size.
  | SelectCases [Text]
  -- ^ Select only a specific case from a @CasesF@.

-- | Simplify the given @Size@, resulting in either the simplified @Size@ or,
--   if it was fully simplified, an explicit upper and lower bound.
szSimplify :: Size -> Either Size (Range Natural)
szSimplify = cata $ \case
  TodoF f pxy -> Left (todo f pxy)
  ValueF x    -> Right (Range {lo = x, hi = x})
  CasesF xs   -> case mapM caseValue xs of
    Right xs' ->
      Right (Range {lo = minimum (map lo xs'), hi = maximum (map hi xs')})
    Left _ -> Left (szCases $ map (fmap toSize) xs)
  AddF x y -> binOp (+) x y
  MulF x y -> binOp (*) x y
  SubF x y -> binOp (-) x y
  NegF x   -> unOp negate x
  AbsF x   -> unOp abs x
  SgnF x   -> unOp signum x
  ApF _ f (Right x) -> Right (Range {lo = f (lo x), hi = f (hi x)})
  ApF n f (Left x) -> Left (apMono n f x)
 where
  binOp
    :: (forall a . Num a => a -> a -> a)
    -> Either Size (Range Natural)
    -> Either Size (Range Natural)
    -> Either Size (Range Natural)
  binOp op (Right x) (Right y) = Right (op x y)
  binOp op x         y         = Left (op (toSize x) (toSize y))

  unOp
    :: (forall a . Num a => a -> a)
    -> Either Size (Range Natural)
    -> Either Size (Range Natural)
  unOp f = \case
    Right x -> Right (f x)
    Left  x -> Left (f x)

  toSize :: Either Size (Range Natural) -> Size
  toSize = \case
    Left  x -> x
    Right r -> if lo r == hi r
      then fromIntegral (lo r)
      else szCases
        [Case "lo" (fromIntegral $ lo r), Case "hi" (fromIntegral $ hi r)]

-- | Force any thunks in the given @Size@ expression.
--
-- > ghci> putStrLn $ pretty $ szForce $ szLazy (Proxy @TxAux)
-- > (0 + { TxAux=(2 + ((0 + (_ :: Tx)) + (_ :: Vector TxInWitness))) })
szForce :: Size -> Size
szForce = cata $ \case
  AddF x y  -> x + y
  MulF x y  -> x * y
  SubF x y  -> x - y
  NegF   x  -> negate x
  AbsF   x  -> abs x
  SgnF   x  -> signum x
  CasesF xs -> Fix $ CasesF xs
  ValueF x  -> Fix (ValueF x)
  ApF n f x -> apMono n f x
  TodoF f x -> f x

szBounds :: Bi a => a -> Either Size (Range Natural)
szBounds = szSimplify . szGreedy . pure
