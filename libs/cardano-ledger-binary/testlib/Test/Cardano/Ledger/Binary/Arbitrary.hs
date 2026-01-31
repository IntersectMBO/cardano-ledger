{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Binary.Arbitrary (
  genVersion,
) where

import Cardano.Ledger.Binary.Version
import qualified Codec.CBOR.ByteArray as CBOR
import Codec.CBOR.ByteArray.Sliced (SlicedByteArray (..))
import Codec.CBOR.Term
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IP (IPv4, IPv6, toIPv4w, toIPv6w)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.VMap as VMap
import Data.Word
import GHC.Stack
import Numeric.Half
import Test.Cardano.Base.Bytes (genByteArray, genByteString, genLazyByteString)
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

deriving instance Arbitrary CBOR.ByteArray

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

instance
  (Ord k, VMap.Vector kv k, VMap.Vector vv v, Arbitrary k, Arbitrary v) =>
  Arbitrary (VMap.VMap kv vv k v)
  where
  arbitrary = VMap.fromMap <$> arbitrary
  shrink = fmap VMap.fromList . shrink . VMap.toList

instance Arbitrary Version where
  arbitrary = genVersion minBound maxBound

genVersion :: HasCallStack => Version -> Version -> Gen Version
genVersion minVersion maxVersion =
  genVersion32 (getVersion32 minVersion) (getVersion32 maxVersion)
  where
    genVersion32 minVersion32 maxVersion32 = do
      v32 <- choose (minVersion32, maxVersion32)
      case mkVersion32 v32 of
        Nothing -> error $ "Impossible: Invalid version generated: " ++ show v32
        Just v -> pure v
