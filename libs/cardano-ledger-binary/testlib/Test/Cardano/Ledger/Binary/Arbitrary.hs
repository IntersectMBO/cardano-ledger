{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Arbitrary (
  genVersion,
  genByteArray,
  genByteString,
  genLazyByteString,
  genShortByteString,
)
where

import Cardano.Crypto.DSIGN.Class hiding (Signable)
import Cardano.Crypto.Util
import Cardano.Crypto.VRF.Class
import Cardano.Ledger.Binary.Version
import Cardano.Slotting.Block (BlockNo (..))
import Cardano.Slotting.Slot (EpochSize (..), WithOrigin (..))
import Cardano.Slotting.Time (SystemStart (..))
import Codec.CBOR.ByteArray (ByteArray (..))
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Codec.CBOR.Term
import qualified Data.ByteString as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromChunks, fromStrict, toChunks)
import Numeric.Half
#if MIN_VERSION_bytestring(0,11,1)
import qualified Data.ByteString.Short as SBS
#else
import qualified Data.ByteString.Short.Internal as SBS
#endif
import qualified Data.Foldable as F
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import Data.Maybe.Strict
import qualified Data.Primitive.ByteArray as Prim (ByteArray (..))
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.VMap as VMap
import Data.Word
import GHC.Stack
import System.Random.Stateful hiding (genByteString, genShortByteString)
import Test.Cardano.Ledger.Binary.Random (QC (QC))
import Test.Cardano.Slotting.Arbitrary ()
import Test.Crypto.Hash ()
import Test.Crypto.KES ()
import Test.Crypto.VRF ()
import Test.QuickCheck
import Test.QuickCheck.Instances ()

firstUnreservedTag :: Word64
firstUnreservedTag = 6

-- | Simple values that are either unassigned or don't have a specialized type already
simple :: [Word8]
simple = [0 .. 19] ++ [23] ++ [32 ..]

instance Arbitrary Term where
  arbitrary =
    oneof
      [ TInt <$> choose (minBound, maxBound)
      , TInteger
          <$> oneof
            [ choose (toInteger (maxBound :: Int) + 1, toInteger (maxBound :: Word64))
            , choose (negate (toInteger (maxBound :: Word64)), toInteger (minBound :: Int) - 1)
            ]
      , TBytes <$> (genByteString . getNonNegative =<< arbitrary)
      , TBytesI <$> (genLazyByteString . getNonNegative =<< arbitrary)
      , TString . T.pack <$> arbitrary
      , TStringI . TL.pack <$> arbitrary
      , TList <$> listOf smallerTerm
      , TListI <$> listOf smallerTerm
      , TMap <$> listOf smallerTerm
      , TMapI <$> listOf smallerTerm
      , TTagged <$> choose (firstUnreservedTag, maxBound :: Word64) <*> smallerTerm
      , TBool <$> arbitrary
      , pure TNull
      , TSimple <$> elements simple
      , THalf <$> genHalf
      , TFloat <$> arbitrary
      , TDouble <$> arbitrary
      ]
    where
      smallerTerm :: Arbitrary a => Gen a
      smallerTerm = scale (`div` 5) arbitrary

  -- Shrinker was shamelessly stolen from cbor package.
  shrink (TInt n) = [TInt n' | n' <- shrink n]
  shrink (TInteger n) = [TInteger n' | n' <- shrink n]
  shrink (TBytes ws) = [TBytes (BS.pack ws') | ws' <- shrink (BS.unpack ws)]
  shrink (TBytesI wss) =
    [ TBytesI (BSL.fromChunks (map BS.pack wss'))
    | wss' <- shrink (map BS.unpack (BSL.toChunks wss))
    ]
  shrink (TString cs) = [TString (T.pack cs') | cs' <- shrink (T.unpack cs)]
  shrink (TStringI css) =
    [ TStringI (TL.fromChunks (map T.pack css'))
    | css' <- shrink (map T.unpack (TL.toChunks css))
    ]
  shrink (TList xs@[x]) = x : [TList xs' | xs' <- shrink xs]
  shrink (TList xs) = [TList xs' | xs' <- shrink xs]
  shrink (TListI xs@[x]) = x : [TListI xs' | xs' <- shrink xs]
  shrink (TListI xs) = [TListI xs' | xs' <- shrink xs]
  shrink (TMap xys@[(x, y)]) = x : y : [TMap xys' | xys' <- shrink xys]
  shrink (TMap xys) = [TMap xys' | xys' <- shrink xys]
  shrink (TMapI xys@[(x, y)]) = x : y : [TMapI xys' | xys' <- shrink xys]
  shrink (TMapI xys) = [TMapI xys' | xys' <- shrink xys]
  shrink (TTagged w t) =
    t : [TTagged w' t' | (w', t') <- shrink (w, t), fromIntegral w' >= firstUnreservedTag]
  shrink (TBool _) = []
  shrink TNull = []
  shrink (TSimple w) = [TSimple w' | w' <- shrink w, w `elem` simple]
  shrink (THalf _f) = []
  shrink (TFloat f) = [TFloat f' | f' <- shrink f]
  shrink (TDouble f) = [TDouble f' | f' <- shrink f]

genHalf :: Gen Float
genHalf = do
  half <- Half <$> arbitrary
  if isInfinite half || isDenormalized half || isNaN half
    then genHalf
    else pure $ fromHalf half

deriving instance Arbitrary ByteArray

instance Arbitrary SlicedByteArray where
  arbitrary = do
    NonNegative off <- arbitrary
    Positive count <- arbitrary
    NonNegative slack <- arbitrary
    let len = off + count + slack
    ba <- genByteArray len
    pure $ SBA ba off count

instance Arbitrary IPv4 where
  arbitrary = toIPv4w <$> arbitrary

instance Arbitrary IPv6 where
  arbitrary = do
    t <- (,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    pure $ toIPv6w t

instance Arbitrary e => Arbitrary (SSeq.StrictSeq e) where
  arbitrary = SSeq.fromList <$> arbitrary
  shrink = fmap SSeq.fromList . shrink . F.toList

instance Arbitrary e => Arbitrary (StrictMaybe e) where
  arbitrary = maybeToStrictMaybe <$> arbitrary
  shrink = fmap maybeToStrictMaybe . shrink . strictMaybeToMaybe

instance
  (Ord k, VMap.Vector kv k, VMap.Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap.VMap kv vv k v)
  where
  arbitrary = VMap.fromMap <$> arbitrary
  shrink = fmap VMap.fromList . shrink . VMap.toList

instance DSIGNAlgorithm v => Arbitrary (VerKeyDSIGN v) where
  arbitrary = deriveVerKeyDSIGN <$> arbitrary

errorInvalidSize :: HasCallStack => Int -> Maybe a -> Gen a
errorInvalidSize n = maybe (error $ "Impossible: Invalid size " ++ show n) pure

instance DSIGNAlgorithm v => Arbitrary (SignKeyDSIGN v) where
  arbitrary = do
    let n = fromIntegral (sizeSignKeyDSIGN (Proxy @v))
    bs <- genByteString n
    errorInvalidSize n $ rawDeserialiseSignKeyDSIGN bs

instance DSIGNAlgorithm v => Arbitrary (SigDSIGN v) where
  arbitrary = do
    let n = fromIntegral (sizeSigDSIGN (Proxy @v))
    bs <- genByteString n
    errorInvalidSize n $ rawDeserialiseSigDSIGN bs

instance DSIGNAlgorithm v => Arbitrary (SignedDSIGN v a) where
  arbitrary = SignedDSIGN <$> arbitrary

instance
  (ContextVRF v ~ (), Signable v ~ SignableRepresentation, VRFAlgorithm v) =>
  Arbitrary (CertifiedVRF v a)
  where
  arbitrary = CertifiedVRF <$> arbitrary <*> genCertVRF
    where
      genCertVRF :: Gen (CertVRF v)
      genCertVRF = arbitrary

instance Arbitrary t => Arbitrary (WithOrigin t) where
  arbitrary = frequency [(20, pure Origin), (80, At <$> arbitrary)]
  shrink = \case
    Origin -> []
    At x -> Origin : map At (shrink x)

deriving instance Arbitrary EpochSize

deriving instance Arbitrary SystemStart

deriving instance Arbitrary BlockNo

instance Arbitrary Version where
  arbitrary = genVersion minBound maxBound

genVersion :: HasCallStack => Version -> Version -> Gen Version
genVersion minVersion maxVersion =
  genVersion64 (getVersion64 minVersion) (getVersion64 maxVersion)
  where
    genVersion64 minVersion64 maxVersion64 = do
      v64 <- choose (minVersion64, maxVersion64)
      case mkVersion64 v64 of
        Nothing -> error $ "Impossible: Invalid version generated: " ++ show v64
        Just v -> pure v

genByteString :: Int -> Gen BS.ByteString
genByteString n = uniformByteStringM (fromIntegral n) QC

genLazyByteString :: Int -> Gen BSL.ByteString
genLazyByteString n = BSL.fromStrict <$> genByteString n

genShortByteString :: Int -> Gen SBS.ShortByteString
genShortByteString n = uniformShortByteString (fromIntegral n) QC

genByteArray :: Int -> Gen Prim.ByteArray
genByteArray n = do
  sbs <- genShortByteString n
  case sbs of
    SBS.SBS ba -> pure (Prim.ByteArray ba)
