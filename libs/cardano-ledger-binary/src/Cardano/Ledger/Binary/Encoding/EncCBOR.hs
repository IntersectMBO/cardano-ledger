{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoStarIsType #-}

module Cardano.Ledger.Binary.Encoding.EncCBOR (
  EncCBOR (..),
  withWordSize,
  PreEncoded (..),
  toByronCBOR,

  -- * Size of expressions
  Range (..),
  szEval,
  Size,
  Case (..),
  caseValue,
  LengthOf (..),
  SizeOverride (..),
  isTodo,
  szCases,
  szLazy,
  szGreedy,
  szForce,
  szWithCtx,
  szSimplify,
  apMono,
  szBounds,

  -- ** Crypto
  encodedVerKeyDSIGNSizeExpr,
  encodedSignKeyDSIGNSizeExpr,
  encodedSigDSIGNSizeExpr,
  encodedSignedDSIGNSizeExpr,
  encodedVerKeyKESSizeExpr,
  encodedSignKeyKESSizeExpr,
  encodedSigKESSizeExpr,
  encodedVerKeyVRFSizeExpr,
  encodedSignKeyVRFSizeExpr,
  encodedCertVRFSizeExpr,
)
where

import Cardano.Crypto.DSIGN.Class (
  DSIGNAlgorithm,
  SeedSizeDSIGN,
  SigDSIGN,
  SignKeyDSIGN,
  SignedDSIGN,
  VerKeyDSIGN,
  sizeSigDSIGN,
  sizeSignKeyDSIGN,
  sizeVerKeyDSIGN,
 )
import Cardano.Crypto.Hash.Class (
  Hash (..),
  HashAlgorithm,
  hashToBytes,
  sizeHash,
 )
import Cardano.Crypto.KES.Class (
  KESAlgorithm,
  OptimizedKESAlgorithm,
  SigKES,
  SignKeyKES,
  VerKeyKES,
  sizeSigKES,
  sizeSignKeyKES,
  sizeVerKeyKES,
 )
import Cardano.Crypto.KES.CompactSingle (CompactSingleKES)
import Cardano.Crypto.KES.CompactSum (CompactSumKES)
import Cardano.Crypto.KES.Mock (MockKES)
import Cardano.Crypto.KES.Simple (SimpleKES)
import Cardano.Crypto.KES.Single (SingleKES)
import Cardano.Crypto.KES.Sum (SumKES)
import Cardano.Crypto.VRF.Class (
  CertVRF,
  CertifiedVRF (..),
  OutputVRF (..),
  SignKeyVRF,
  VRFAlgorithm,
  VerKeyVRF,
  sizeCertVRF,
  sizeOutputVRF,
  sizeSignKeyVRF,
  sizeVerKeyVRF,
 )
import Cardano.Crypto.VRF.Mock (MockVRF)
import qualified Cardano.Crypto.VRF.Praos as Praos
import Cardano.Crypto.VRF.Simple (SimpleVRF)
import Cardano.Ledger.Binary.Crypto
import Cardano.Ledger.Binary.Encoding.Encoder
import Cardano.Ledger.Binary.Version (Version, byronProtVer, getVersion64)
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (
  EpochInterval (..),
  EpochNo (..),
  EpochSize (..),
  SlotNo (..),
  WithOrigin (..),
 )
import Cardano.Slotting.Time (SystemStart (..))
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (SBA), fromByteArray)
import qualified Codec.CBOR.Encoding as C (Encoding (..))
import Codec.CBOR.Term (Term (..))
import qualified Codec.Serialise as Serialise (Serialise (encode))
import Control.Category (Category ((.)))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.ByteString.Short as SBS (length)
#if MIN_VERSION_bytestring(0,11,1)
import Data.ByteString.Short (ShortByteString(SBS))
#else
import Data.ByteString.Short.Internal (ShortByteString(SBS))
#endif
import qualified Cardano.Binary as Plain (Encoding, ToCBOR (..))
import Data.Fixed (Fixed (..))
import Data.Foldable (toList)
import Data.Functor.Foldable (cata, project)
import Data.IP (IPv4, IPv6)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe.Strict as SMaybe
import qualified Data.Primitive.ByteArray as Prim (ByteArray (..))
import Data.Ratio (Ratio)
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy.Builder (Builder)
import Data.Time.Clock (UTCTime (..))
import Data.Typeable (Proxy (..), TypeRep, Typeable, typeRep)
import qualified Data.VMap as VMap
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Storable (sizeOf)
import Formatting (bprint, build, shown, stext)
import qualified Formatting.Buildable as B (Buildable (..))
import GHC.TypeNats (KnownNat, type (*))
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as PV1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusLedgerApi.V3 as PV3
import Prelude hiding (encodeFloat, (.))

#if MIN_VERSION_recursion_schemes(5,2,0)
import Data.Fix (Fix(..))
#else
import Data.Functor.Foldable (Fix(..))
#endif

class Typeable a => EncCBOR a where
  encCBOR :: a -> Encoding
  default encCBOR :: Plain.ToCBOR a => a -> Encoding
  encCBOR = fromPlainEncoding . Plain.toCBOR

  encodedSizeExpr :: (forall t. EncCBOR t => Proxy t -> Size) -> Proxy a -> Size
  encodedSizeExpr = todo

  encodedListSizeExpr :: (forall t. EncCBOR t => Proxy t -> Size) -> Proxy [a] -> Size
  encodedListSizeExpr = defaultEncodedListSizeExpr

-- | A type used to represent the length of a value in 'Size' computations.
newtype LengthOf xs = LengthOf xs

instance Typeable xs => EncCBOR (LengthOf xs) where
  encCBOR = error "The `LengthOf` type cannot be encoded!"

-- | Default size expression for a list type.
defaultEncodedListSizeExpr ::
  forall a.
  EncCBOR a =>
  (forall t. EncCBOR t => Proxy t -> Size) ->
  Proxy [a] ->
  Size
defaultEncodedListSizeExpr size _ =
  2 + size (Proxy @(LengthOf [a])) * size (Proxy @a)

newtype PreEncoded = PreEncoded {unPreEncoded :: BS.ByteString}

instance EncCBOR PreEncoded where
  encCBOR = encodePreEncoded . unPreEncoded

instance EncCBOR Version where
  encCBOR = encodeVersion
  encodedSizeExpr f px = f (getVersion64 <$> px)

-- | Convert a versioned `EncCBOR` instance to a plain `Plain.Encoding` using Byron
-- protocol version.
toByronCBOR :: EncCBOR a => a -> Plain.Encoding
toByronCBOR = toPlainEncoding byronProtVer . encCBOR

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
  = -- | Sum of two sizes.
    AddF t t
  | -- | Product of two sizes.
    MulF t t
  | -- | Difference of two sizes.
    SubF t t
  | -- | Absolute value of a size.
    AbsF t
  | -- | Negation of a size.
    NegF t
  | -- | Signum of a size.
    SgnF t
  | -- | Case-selection for sizes. Used for sum types.
    CasesF [Case t]
  | -- | A constant value.
    ValueF Natural
  | -- | Application of a monotonic function to a size.
    ApF Text (Natural -> Natural) t
  | -- | A suspended size calculation ("thunk"). This is used to delay the
    --   computation of a size until some later point, which is useful for
    --   progressively building more detailed size estimates for a type
    --   from the outside in. For example, `szLazy` can be followed by
    --   applications of `szForce` to reveal more detailed expressions
    --   describing the size bounds on a type.
    forall a. EncCBOR a => TodoF (forall x. EncCBOR x => Proxy x -> Size) (Proxy a)

instance Functor SizeF where
  fmap f = \case
    AddF x y -> AddF (f x) (f y)
    MulF x y -> MulF (f x) (f y)
    SubF x y -> SubF (f x) (f y)
    AbsF x -> AbsF (f x)
    NegF x -> NegF (f x)
    SgnF x -> SgnF (f x)
    CasesF xs -> CasesF (map (fmap f) xs)
    ValueF x -> ValueF x
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
  build x_ =
    let showp2 :: (B.Buildable a, B.Buildable b) => a -> Text -> b -> Builder
        showp2 = bprint ("(" . build . " " . stext . " " . build . ")")
     in case x_ of
          AddF x y -> showp2 x "+" y
          MulF x y -> showp2 x "*" y
          SubF x y -> showp2 x "-" y
          NegF x -> bprint ("-" . build) x
          AbsF x -> bprint ("|" . build . "|") x
          SgnF x -> bprint ("sgn(" . build . ")") x
          CasesF xs ->
            bprint ("{ " . build . "}") $ foldMap (bprint (build . " ")) xs
          ValueF x -> bprint shown (toInteger x)
          ApF n _ x -> bprint (stext . "(" . build . ")") n x
          TodoF _ x -> bprint ("(_ :: " . shown . ")") (typeRep x)

instance B.Buildable (Fix SizeF) where
  build x = bprint build (project @(Fix _) x)

-- | Create a case expression from individual cases.
szCases :: [Case Size] -> Size
szCases = Fix . CasesF

-- | An individual labeled case.
data Case t
  = Case Text t
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
    let products = [u * v | u <- [lo x, hi x], v <- [lo y, hi y]]
     in Range {lo = minimum products, hi = maximum products}
  x - y = Range {lo = lo x - hi y, hi = hi x - lo y}
  negate x = Range {lo = negate (hi x), hi = negate (lo x)}
  abs x =
    if
      | lo x <= 0 && hi x >= 0 -> Range {lo = 0, hi = max (hi x) (negate $ lo x)}
      | lo x <= 0 && hi x <= 0 -> Range {lo = negate (hi x), hi = negate (lo x)}
      | otherwise -> x
  signum x = Range {lo = signum (lo x), hi = signum (hi x)}
  fromInteger n = Range {lo = fromInteger n, hi = fromInteger n}

instance B.Buildable (Range Natural) where
  build r = bprint (shown . ".." . shown) (toInteger $ lo r) (toInteger $ hi r)

-- | Fully evaluate a size expression by applying the given function to any
--   suspended computations. @szEval g@ effectively turns each "thunk"
--   of the form @TodoF f x@ into @g x@, then evaluates the result.
szEval ::
  (forall t. EncCBOR t => (Proxy t -> Size) -> Proxy t -> Range Natural) ->
  Size ->
  Range Natural
szEval doit = cata $ \case
  AddF x y -> x + y
  MulF x y -> x * y
  SubF x y -> x - y
  NegF x -> negate x
  AbsF x -> abs x
  SgnF x -> signum x
  CasesF xs ->
    Range
      { lo = minimum (map (lo . caseValue) xs)
      , hi = maximum (map (hi . caseValue) xs)
      }
  ValueF x -> Range {lo = x, hi = x}
  ApF _ f x -> Range {lo = f (lo x), hi = f (hi x)}
  TodoF f x -> doit f x

-- | Evaluate the expression lazily, by immediately creating a thunk
--    that will evaluate its contents lazily.
--
-- > ghci> putStrLn $ pretty $ szLazy (Proxy @TxAux)
-- > (_ :: TxAux)
szLazy :: EncCBOR a => (Proxy a -> Size)
szLazy = todo (encodedSizeExpr szLazy)

-- | Evaluate an expression greedily. There may still be thunks in the
--    result, for types that did not provide a custom 'encodedSizeExpr' method
--    in their 'EncCBOR' instance.
--
-- > ghci> putStrLn $ pretty $ szGreedy (Proxy @TxAux)
-- > (0 + { TxAux=(2 + ((0 + (((1 + (2 + ((_ :: LengthOf [TxIn]) * (2 + { TxInUtxo=(2 + ((1 + 34) + { minBound=1 maxBound=5 })) })))) + (2 + ((_ :: LengthOf [TxOut]) * (0 + { TxOut=(2 + ((0 + ((2 + ((2 + withWordSize((((1 + 30) + (_ :: Attributes AddrAttributes)) + 1))) + (((1 + 30) + (_ :: Attributes AddrAttributes)) + 1))) + { minBound=1 maxBound=5 })) + { minBound=1 maxBound=9 })) })))) + (_ :: Attributes ()))) + (_ :: Vector TxInWitness))) })
szGreedy :: EncCBOR a => (Proxy a -> Size)
szGreedy = encodedSizeExpr szGreedy

-- | Is this expression a thunk?
isTodo :: Size -> Bool
isTodo (Fix (TodoF _ _)) = True
isTodo _ = False

-- | Create a "thunk" that will apply @f@ to @pxy@ when forced.
todo ::
  forall a.
  EncCBOR a =>
  (forall t. EncCBOR t => Proxy t -> Size) ->
  Proxy a ->
  Size
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
szWithCtx :: EncCBOR a => Map.Map TypeRep SizeOverride -> Proxy a -> Size
szWithCtx ctx pxy = case Map.lookup (typeRep pxy) ctx of
  Nothing -> normal
  Just override -> case override of
    SizeConstant sz -> sz
    SizeExpression f -> f (szWithCtx ctx)
    SelectCases names -> cata (selectCase names) normal
  where
    -- The non-override case
    normal = encodedSizeExpr (szWithCtx ctx) pxy

    selectCase :: [Text] -> SizeF Size -> Size
    selectCase names orig = case orig of
      CasesF cs -> matchCase names cs (Fix orig)
      _ -> Fix orig

    matchCase :: [Text] -> [Case Size] -> Size -> Size
    matchCase names cs orig =
      case filter (\(Case name _) -> name `elem` names) cs of
        [] -> orig
        [Case _ x] -> x
        cs' -> Fix (CasesF cs')

-- | Override mechanisms to be used with 'szWithCtx'.
data SizeOverride
  = -- | Replace with a fixed @Size@.
    SizeConstant Size
  | -- | Recursively compute the size.
    SizeExpression ((forall a. EncCBOR a => Proxy a -> Size) -> Size)
  | -- | Select only a specific case from a @CasesF@.
    SelectCases [Text]

-- | Simplify the given @Size@, resulting in either the simplified @Size@ or,
--   if it was fully simplified, an explicit upper and lower bound.
szSimplify :: Size -> Either Size (Range Natural)
szSimplify = cata $ \case
  TodoF f pxy -> Left (todo f pxy)
  ValueF x -> Right (Range {lo = x, hi = x})
  CasesF xs -> case mapM caseValue xs of
    Right xs' ->
      Right (Range {lo = minimum (map lo xs'), hi = maximum (map hi xs')})
    Left _ -> Left (szCases $ map (fmap toSize) xs)
  AddF x y -> binOp (+) x y
  MulF x y -> binOp (*) x y
  SubF x y -> binOp (-) x y
  NegF x -> unOp negate x
  AbsF x -> unOp abs x
  SgnF x -> unOp signum x
  ApF _ f (Right x) -> Right (Range {lo = f (lo x), hi = f (hi x)})
  ApF n f (Left x) -> Left (apMono n f x)
  where
    binOp ::
      (forall a. Num a => a -> a -> a) ->
      Either Size (Range Natural) ->
      Either Size (Range Natural) ->
      Either Size (Range Natural)
    binOp op (Right x) (Right y) = Right (op x y)
    binOp op x y = Left (op (toSize x) (toSize y))

    unOp ::
      (forall a. Num a => a -> a) ->
      Either Size (Range Natural) ->
      Either Size (Range Natural)
    unOp f = \case
      Right x -> Right (f x)
      Left x -> Left (f x)

    toSize :: Either Size (Range Natural) -> Size
    toSize = \case
      Left x -> x
      Right r ->
        if lo r == hi r
          then fromIntegral (lo r)
          else
            szCases
              [Case "lo" (fromIntegral $ lo r), Case "hi" (fromIntegral $ hi r)]

-- | Force any thunks in the given @Size@ expression.
--
-- > ghci> putStrLn $ pretty $ szForce $ szLazy (Proxy @TxAux)
-- > (0 + { TxAux=(2 + ((0 + (_ :: Tx)) + (_ :: Vector TxInWitness))) })
szForce :: Size -> Size
szForce = cata $ \case
  AddF x y -> x + y
  MulF x y -> x * y
  SubF x y -> x - y
  NegF x -> negate x
  AbsF x -> abs x
  SgnF x -> signum x
  CasesF xs -> Fix $ CasesF xs
  ValueF x -> Fix (ValueF x)
  ApF n f x -> apMono n f x
  TodoF f x -> f x

szBounds :: EncCBOR a => a -> Either Size (Range Natural)
szBounds = szSimplify . szGreedy . pure

-- | Compute encoded size of an integer
withWordSize :: (Integral s, Integral a) => s -> a
withWordSize x =
  let s = fromIntegral x :: Integer
   in if
        | s <= 0x17 && s >= (-0x18) -> 1
        | s <= 0xff && s >= (-0x100) -> 2
        | s <= 0xffff && s >= (-0x10000) -> 3
        | s <= 0xffffffff && s >= (-0x100000000) -> 5
        | otherwise -> 9

--------------------------------------------------------------------------------
-- Primitive types
--------------------------------------------------------------------------------

instance EncCBOR () where
  encCBOR = const encodeNull
  encodedSizeExpr _ _ = 1

instance EncCBOR Bool where
  encCBOR = encodeBool
  encodedSizeExpr _ _ = 1

--------------------------------------------------------------------------------
-- Numeric data
--------------------------------------------------------------------------------

instance EncCBOR Integer where
  encCBOR = encodeInteger

encodedSizeRange :: forall a. (Integral a, Bounded a) => Proxy a -> Size
encodedSizeRange _ =
  szCases
    [ mkCase "minBound" 0 -- min, in absolute value
    , mkCase "maxBound" maxBound
    ]
  where
    mkCase :: Text -> a -> Case Size
    mkCase n = Case n . fromInteger . withWordSize

instance EncCBOR Word where
  encCBOR = encodeWord
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Word8 where
  encCBOR = encodeWord8
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Word16 where
  encCBOR = encodeWord16
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Word32 where
  encCBOR = encodeWord32
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Word64 where
  encCBOR = encodeWord64
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Int where
  encCBOR = encodeInt
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Int8 where
  encCBOR = encodeInt8
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Int16 where
  encCBOR = encodeInt16
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Int32 where
  encCBOR = encodeInt32
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Int64 where
  encCBOR = encodeInt64
  encodedSizeExpr _ = encodedSizeRange

instance EncCBOR Float where
  encCBOR = encodeFloat
  encodedSizeExpr _ _ = 1 + fromIntegral (sizeOf (0 :: Float))

instance EncCBOR Double where
  encCBOR = encodeDouble
  encodedSizeExpr _ _ = 1 + fromIntegral (sizeOf (0 :: Float))

instance EncCBOR a => EncCBOR (Ratio a) where
  encCBOR = encodeRatio encCBOR
  encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @a)

deriving newtype instance Typeable p => EncCBOR (Fixed p)

instance EncCBOR Natural where
  encCBOR = encCBOR . toInteger

instance EncCBOR Void where
  encCBOR = absurd

instance EncCBOR IPv4 where
  encCBOR = encodeIPv4

instance EncCBOR IPv6 where
  encCBOR = encodeIPv6

--------------------------------------------------------------------------------
-- CBOR
--------------------------------------------------------------------------------

instance EncCBOR Term where
  encCBOR = encodeTerm

instance EncCBOR Encoding where
  encCBOR = id

instance EncCBOR C.Encoding where
  encCBOR = fromPlainEncoding

instance EncCBOR (Tokens -> Tokens) where
  encCBOR t = fromPlainEncoding (C.Encoding t)

--------------------------------------------------------------------------------
-- Tagged
--------------------------------------------------------------------------------

instance (Typeable s, EncCBOR a) => EncCBOR (Tagged s a) where
  encCBOR (Tagged a) = encCBOR a

  encodedSizeExpr size _ = encodedSizeExpr size (Proxy @a)

--------------------------------------------------------------------------------
-- Containers
--------------------------------------------------------------------------------

instance (EncCBOR a, EncCBOR b) => EncCBOR (a, b) where
  encCBOR (a, b) = encodeListLen 2 <> encCBOR a <> encCBOR b

  encodedSizeExpr size _ = 1 + size (Proxy @a) + size (Proxy @b)

instance (EncCBOR a, EncCBOR b, EncCBOR c) => EncCBOR (a, b, c) where
  encCBOR (a, b, c) = encodeListLen 3 <> encCBOR a <> encCBOR b <> encCBOR c

  encodedSizeExpr size _ =
    1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c)

instance (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d) => EncCBOR (a, b, c, d) where
  encCBOR (a, b, c, d) =
    encodeListLen 4 <> encCBOR a <> encCBOR b <> encCBOR c <> encCBOR d

  encodedSizeExpr size _ =
    1 + size (Proxy @a) + size (Proxy @b) + size (Proxy @c) + size (Proxy @d)

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e) =>
  EncCBOR (a, b, c, d, e)
  where
  encCBOR (a, b, c, d, e) =
    encodeListLen 5
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e

  encodedSizeExpr size _ =
    1
      + size (Proxy @a)
      + size (Proxy @b)
      + size (Proxy @c)
      + size (Proxy @d)
      + size (Proxy @e)

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e, EncCBOR f) =>
  EncCBOR (a, b, c, d, e, f)
  where
  encCBOR (a, b, c, d, e, f) =
    encodeListLen 6
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e
      <> encCBOR f

  encodedSizeExpr size _ =
    1
      + size (Proxy @a)
      + size (Proxy @b)
      + size (Proxy @c)
      + size (Proxy @d)
      + size (Proxy @e)
      + size (Proxy @f)

instance
  (EncCBOR a, EncCBOR b, EncCBOR c, EncCBOR d, EncCBOR e, EncCBOR f, EncCBOR g) =>
  EncCBOR (a, b, c, d, e, f, g)
  where
  encCBOR (a, b, c, d, e, f, g) =
    encodeListLen 7
      <> encCBOR a
      <> encCBOR b
      <> encCBOR c
      <> encCBOR d
      <> encCBOR e
      <> encCBOR f
      <> encCBOR g

  encodedSizeExpr size _ =
    1
      + size (Proxy @a)
      + size (Proxy @b)
      + size (Proxy @c)
      + size (Proxy @d)
      + size (Proxy @e)
      + size (Proxy @f)
      + size (Proxy @g)

instance EncCBOR BS.ByteString where
  encCBOR = encodeBytes
  encodedSizeExpr size _ =
    let len = size (Proxy @(LengthOf BS.ByteString))
     in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance EncCBOR Text.Text where
  encCBOR = encodeString
  encodedSizeExpr size _ =
    let bsLength =
          size (Proxy @(LengthOf Text))
            * szCases [Case "minChar" 1, Case "maxChar" 4]
     in bsLength + apMono "withWordSize" withWordSize bsLength

instance EncCBOR ByteArray where
  encCBOR = encCBOR . unBA
  {-# INLINE encCBOR #-}

instance EncCBOR Prim.ByteArray where
  encCBOR = encodeByteArray . fromByteArray
  {-# INLINE encCBOR #-}

instance EncCBOR SlicedByteArray where
  encCBOR = encodeByteArray
  {-# INLINE encCBOR #-}

instance EncCBOR ShortByteString where
  encCBOR sbs@(SBS ba) =
    encodeByteArray $ SBA (Prim.ByteArray ba) 0 (SBS.length sbs)

  encodedSizeExpr size _ =
    let len = size (Proxy @(LengthOf ShortByteString))
     in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance EncCBOR BS.Lazy.ByteString where
  encCBOR = encCBOR . BS.Lazy.toStrict
  encodedSizeExpr size _ =
    let len = size (Proxy @(LengthOf BS.Lazy.ByteString))
     in apMono "withWordSize@Int" (withWordSize @Int . fromIntegral) len + len

instance EncCBOR a => EncCBOR [a] where
  encCBOR = encodeList encCBOR
  encodedSizeExpr size _ = encodedListSizeExpr size (Proxy @[a])

instance (EncCBOR a, EncCBOR b) => EncCBOR (Either a b) where
  encCBOR (Left x) = encodeListLen 2 <> encodeWord 0 <> encCBOR x
  encCBOR (Right x) = encodeListLen 2 <> encodeWord 1 <> encCBOR x

  encodedSizeExpr size _ =
    szCases
      [Case "Left" (2 + size (Proxy @a)), Case "Right" (2 + size (Proxy @b))]

instance EncCBOR a => EncCBOR (NonEmpty a) where
  encCBOR = encCBOR . toList
  encodedSizeExpr size _ = size (Proxy @[a]) -- MN TODO make 0 count impossible

instance EncCBOR a => EncCBOR (Maybe a) where
  encCBOR = encodeMaybe encCBOR

  encodedSizeExpr size _ =
    szCases [Case "Nothing" 1, Case "Just" (1 + size (Proxy @a))]

instance EncCBOR a => EncCBOR (SMaybe.StrictMaybe a) where
  encCBOR = encodeStrictMaybe encCBOR

  encodedSizeExpr size _ =
    szCases [Case "SNothing" 1, Case "SJust" (1 + size (Proxy @a))]

instance (Ord k, EncCBOR k, EncCBOR v) => EncCBOR (Map.Map k v) where
  encCBOR = encodeMap encCBOR encCBOR

instance (Ord a, EncCBOR a) => EncCBOR (Set.Set a) where
  encCBOR = encodeSet encCBOR

instance EncCBOR a => EncCBOR (Seq.Seq a) where
  encCBOR = encodeSeq encCBOR

instance EncCBOR a => EncCBOR (SSeq.StrictSeq a) where
  encCBOR = encodeStrictSeq encCBOR

instance
  (Ord k, EncCBOR k, EncCBOR v, VMap.Vector kv k, VMap.Vector vv v, Typeable kv, Typeable vv) =>
  EncCBOR (VMap.VMap kv vv k v)
  where
  encCBOR = encodeVMap encCBOR encCBOR

instance EncCBOR a => EncCBOR (V.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}
  encodedSizeExpr size _ =
    2 + size (Proxy @(LengthOf (V.Vector a))) * size (Proxy @a)

instance (EncCBOR a, VP.Prim a) => EncCBOR (VP.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}
  encodedSizeExpr size _ =
    2 + size (Proxy @(LengthOf (VP.Vector a))) * size (Proxy @a)

instance (EncCBOR a, VS.Storable a) => EncCBOR (VS.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}
  encodedSizeExpr size _ =
    2 + size (Proxy @(LengthOf (VS.Vector a))) * size (Proxy @a)

instance (EncCBOR a, VU.Unbox a) => EncCBOR (VU.Vector a) where
  encCBOR = encodeVector encCBOR
  {-# INLINE encCBOR #-}
  encodedSizeExpr size _ =
    2 + size (Proxy @(LengthOf (VU.Vector a))) * size (Proxy @a)

--------------------------------------------------------------------------------
-- Time
--------------------------------------------------------------------------------

instance EncCBOR UTCTime where
  encCBOR = encodeUTCTime

--------------------------------------------------------------------------------
-- Crypto
--------------------------------------------------------------------------------

-- | 'Size' expression for 'VerKeyDSIGN' which is using 'sizeVerKeyDSIGN'
-- encoded as 'Size'.
encodedVerKeyDSIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (VerKeyDSIGN v) -> Size
encodedVerKeyDSIGNSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeVerKeyDSIGN (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeVerKeyDSIGN (Proxy :: Proxy v))

-- | 'Size' expression for 'SignKeyDSIGN' which is using 'sizeSignKeyDSIGN'
-- encoded as 'Size'.
encodedSignKeyDSIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (SignKeyDSIGN v) -> Size
encodedSignKeyDSIGNSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeSignKeyDSIGN (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeSignKeyDSIGN (Proxy :: Proxy v))

-- | 'Size' expression for 'SigDSIGN' which is using 'sizeSigDSIGN' encoded as
-- 'Size'.
encodedSigDSIGNSizeExpr :: forall v. DSIGNAlgorithm v => Proxy (SigDSIGN v) -> Size
encodedSigDSIGNSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeSigDSIGN (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeSigDSIGN (Proxy :: Proxy v))

-- | 'Size' expression for 'SignedDSIGN' which uses `encodedSigDSIGNSizeExpr`
encodedSignedDSIGNSizeExpr :: forall v a. DSIGNAlgorithm v => Proxy (SignedDSIGN v a) -> Size
encodedSignedDSIGNSizeExpr _proxy = encodedSigDSIGNSizeExpr (Proxy :: Proxy (SigDSIGN v))

--------------------------------------------------------------------------------
-- DSIGN
--------------------------------------------------------------------------------

instance DSIGNAlgorithm v => EncCBOR (VerKeyDSIGN v) where
  encCBOR = encodeVerKeyDSIGN
  encodedSizeExpr _ = encodedVerKeyDSIGNSizeExpr

instance DSIGNAlgorithm v => EncCBOR (SignKeyDSIGN v) where
  encCBOR = encodeSignKeyDSIGN
  encodedSizeExpr _ = encodedSignKeyDSIGNSizeExpr

instance DSIGNAlgorithm v => EncCBOR (SigDSIGN v) where
  encCBOR = encodeSigDSIGN
  encodedSizeExpr _ = encodedSigDSIGNSizeExpr

instance (DSIGNAlgorithm v, Typeable a) => EncCBOR (SignedDSIGN v a) where
  encCBOR = encodeSignedDSIGN
  encodedSizeExpr _ = encodedSignedDSIGNSizeExpr

--------------------------------------------------------------------------------
-- Hash
--------------------------------------------------------------------------------

instance (HashAlgorithm h, Typeable a) => EncCBOR (Hash h a) where
  encCBOR (UnsafeHash h) = encCBOR h

  -- \| 'Size' expression for @Hash h a@, which is expressed using the 'EncCBOR'
  -- instance for 'ByteString' (as is the above 'encCBOR' method).  'Size'
  -- computation of length of the bytestring is passed as the first argument to
  -- 'encodedSizeExpr'.  The 'ByteString' instance will use it to calculate
  -- @'size' ('Proxy' @('LengthOf' 'ByteString'))@.
  encodedSizeExpr _size proxy =
    encodedSizeExpr (const hashSize) (hashToBytes <$> proxy)
    where
      hashSize :: Size
      hashSize = fromIntegral (sizeHash (Proxy :: Proxy h))

--------------------------------------------------------------------------------
-- KES
--------------------------------------------------------------------------------

-- | 'Size' expression for 'VerKeyKES' which is using 'sizeVerKeyKES' encoded
-- as 'Size'.
encodedVerKeyKESSizeExpr :: forall v. KESAlgorithm v => Proxy (VerKeyKES v) -> Size
encodedVerKeyKESSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeVerKeyKES (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeVerKeyKES (Proxy :: Proxy v))

-- | 'Size' expression for 'SignKeyKES' which is using 'sizeSignKeyKES' encoded
-- as 'Size'.
encodedSignKeyKESSizeExpr :: forall v. KESAlgorithm v => Proxy (SignKeyKES v) -> Size
encodedSignKeyKESSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeSignKeyKES (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeSignKeyKES (Proxy :: Proxy v))

-- | 'Size' expression for 'SigKES' which is using 'sizeSigKES' encoded as
-- 'Size'.
encodedSigKESSizeExpr :: forall v. KESAlgorithm v => Proxy (SigKES v) -> Size
encodedSigKESSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeSigKES (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeSigKES (Proxy :: Proxy v))

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  EncCBOR (VerKeyKES (SimpleKES d t))
  where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  EncCBOR (SignKeyKES (SimpleKES d t))
  where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance
  (DSIGNAlgorithm d, KnownNat t, KnownNat (SeedSizeDSIGN d * t)) =>
  EncCBOR (SigKES (SimpleKES d t))
  where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance
  (KESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (VerKeyKES (SumKES h d))
  where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance
  (KESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (SignKeyKES (SumKES h d))
  where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance
  (KESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (SigKES (SumKES h d))
  where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (VerKeyKES (CompactSingleKES d)) where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (SignKeyKES (CompactSingleKES d)) where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (SigKES (CompactSingleKES d)) where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (VerKeyKES (CompactSumKES h d))
  where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (SignKeyKES (CompactSumKES h d))
  where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance
  (OptimizedKESAlgorithm d, HashAlgorithm h) =>
  EncCBOR (SigKES (CompactSumKES h d))
  where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (VerKeyKES (SingleKES d)) where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (SignKeyKES (SingleKES d)) where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance DSIGNAlgorithm d => EncCBOR (SigKES (SingleKES d)) where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

instance KnownNat t => EncCBOR (VerKeyKES (MockKES t)) where
  encCBOR = encodeVerKeyKES
  encodedSizeExpr _size = encodedVerKeyKESSizeExpr

instance KnownNat t => EncCBOR (SignKeyKES (MockKES t)) where
  encCBOR = encodeSignKeyKES
  encodedSizeExpr _size = encodedSignKeyKESSizeExpr

instance KnownNat t => EncCBOR (SigKES (MockKES t)) where
  encCBOR = encodeSigKES
  encodedSizeExpr _size = encodedSigKESSizeExpr

--------------------------------------------------------------------------------
-- VRF
--------------------------------------------------------------------------------

-- | 'Size' expression for 'VerKeyVRF' which is using 'sizeVerKeyVRF' encoded as
-- 'Size'.
encodedVerKeyVRFSizeExpr :: forall v. VRFAlgorithm v => Proxy (VerKeyVRF v) -> Size
encodedVerKeyVRFSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeVerKeyVRF (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeVerKeyVRF (Proxy :: Proxy v))

-- | 'Size' expression for 'SignKeyVRF' which is using 'sizeSignKeyVRF' encoded
-- as 'Size'
encodedSignKeyVRFSizeExpr :: forall v. VRFAlgorithm v => Proxy (SignKeyVRF v) -> Size
encodedSignKeyVRFSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeSignKeyVRF (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeSignKeyVRF (Proxy :: Proxy v))

-- | 'Size' expression for 'CertVRF' which is using 'sizeCertVRF' encoded as
-- 'Size'.
encodedCertVRFSizeExpr :: forall v. VRFAlgorithm v => Proxy (CertVRF v) -> Size
encodedCertVRFSizeExpr _proxy =
  -- 'encodeBytes' envelope
  fromIntegral ((withWordSize :: Word -> Integer) (sizeCertVRF (Proxy :: Proxy v)))
    -- payload
    + fromIntegral (sizeCertVRF (Proxy :: Proxy v))

instance EncCBOR (VerKeyVRF SimpleVRF) where
  encCBOR = encodeVerKeyVRF
  encodedSizeExpr _size = encodedVerKeyVRFSizeExpr

instance EncCBOR (SignKeyVRF SimpleVRF) where
  encCBOR = encodeSignKeyVRF
  encodedSizeExpr _size = encodedSignKeyVRFSizeExpr

instance EncCBOR (CertVRF SimpleVRF) where
  encCBOR = encodeCertVRF
  encodedSizeExpr _size = encodedCertVRFSizeExpr

instance EncCBOR (VerKeyVRF MockVRF) where
  encCBOR = encodeVerKeyVRF
  encodedSizeExpr _size = encodedVerKeyVRFSizeExpr

instance EncCBOR (SignKeyVRF MockVRF) where
  encCBOR = encodeSignKeyVRF
  encodedSizeExpr _size = encodedSignKeyVRFSizeExpr

instance EncCBOR (CertVRF MockVRF) where
  encCBOR = encodeCertVRF
  encodedSizeExpr _size = encodedCertVRFSizeExpr

deriving instance Typeable v => EncCBOR (OutputVRF v)

instance (VRFAlgorithm v, Typeable a) => EncCBOR (CertifiedVRF v a) where
  encCBOR cvrf =
    encodeListLen 2
      <> encCBOR (certifiedOutput cvrf)
      <> encodeCertVRF (certifiedProof cvrf)

  encodedSizeExpr _size proxy =
    1
      + certifiedOutputSize (certifiedOutput <$> proxy)
      + fromIntegral (sizeCertVRF (Proxy :: Proxy v))
    where
      certifiedOutputSize :: Proxy (OutputVRF v) -> Size
      certifiedOutputSize _proxy =
        fromIntegral $ sizeOutputVRF (Proxy :: Proxy v)

instance EncCBOR Praos.Proof where
  encCBOR = encCBOR . Praos.proofBytes
  encodedSizeExpr _ _ =
    encodedSizeExpr (\_ -> fromIntegral Praos.certSizeVRF) (Proxy :: Proxy BS.ByteString)

instance EncCBOR Praos.SignKey where
  encCBOR = encCBOR . Praos.skBytes
  encodedSizeExpr _ _ =
    encodedSizeExpr (\_ -> fromIntegral Praos.signKeySizeVRF) (Proxy :: Proxy BS.ByteString)

instance EncCBOR Praos.VerKey where
  encCBOR = encCBOR . Praos.vkBytes
  encodedSizeExpr _ _ =
    encodedSizeExpr (\_ -> fromIntegral Praos.verKeySizeVRF) (Proxy :: Proxy BS.ByteString)

deriving instance EncCBOR (VerKeyVRF Praos.PraosVRF)

deriving instance EncCBOR (SignKeyVRF Praos.PraosVRF)

deriving instance EncCBOR (CertVRF Praos.PraosVRF)

--------------------------------------------------------------------------------
-- Slotting
--------------------------------------------------------------------------------

-- TODO: Remove usage of 'serialise' package
instance EncCBOR SlotNo where
  encCBOR = fromPlainEncoding . Serialise.encode
  encodedSizeExpr size = encodedSizeExpr size . fmap unSlotNo

instance (Serialise.Serialise t, Typeable t) => EncCBOR (WithOrigin t) where
  encCBOR = fromPlainEncoding . Serialise.encode

deriving instance EncCBOR EpochNo

deriving instance EncCBOR EpochSize

deriving instance EncCBOR SystemStart

instance EncCBOR BlockNo where
  encCBOR = fromPlainEncoding . Serialise.encode
  encodedSizeExpr size = encodedSizeExpr size . fmap unBlockNo

deriving instance EncCBOR EpochInterval

--------------------------------------------------------------------------------
-- Plutus
--------------------------------------------------------------------------------

instance EncCBOR PV1.Data where
  encCBOR = fromPlainEncoding . Serialise.encode

instance EncCBOR PV1.ScriptContext where
  encCBOR = encCBOR . PV3.toData

instance EncCBOR PV2.ScriptContext where
  encCBOR = encCBOR . PV3.toData

instance EncCBOR PV3.ScriptContext where
  encCBOR = encCBOR . PV3.toData
