{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.Address.CompactAddr where

import qualified Cardano.Crypto.Hash.Class as Hash
import Cardano.Ledger.Address (Addr (..), putVariableLengthWord64, serialiseAddr)
import qualified Cardano.Ledger.CompactAddress as CA
import Cardano.Ledger.Credential
import qualified Cardano.Ledger.Crypto as CC (Crypto (ADDRHASH))
import qualified Data.Binary.Put as B
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Either
import Data.Maybe (isJust)
import Data.Proxy
import Data.Word
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck
import Test.QuickCheck.Gen (chooseWord64)

propValidateNewDecompact :: forall crypto. CC.Crypto crypto => Addr crypto -> Property
propValidateNewDecompact addr =
  let compact = SBS.toShort $ serialiseAddr addr
      decompactedOld = CA.deserializeShortAddr @crypto compact
      decompactedNew = CA.decodeAddrShort @crypto compact
   in isJust decompactedOld .&&. decompactedOld === decompactedNew

propCompactAddrRoundTrip :: CC.Crypto crypto => Addr crypto -> Bool
propCompactAddrRoundTrip addr =
  let compact = CA.compactAddr addr
      decompact = CA.decompactAddr compact
   in addr == decompact

propCompactSerializationAgree :: Addr crypto -> Bool
propCompactSerializationAgree addr =
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
   in sbs == SBS.toShort (serialiseAddr addr)

propDecompactErrors :: forall crypto. CC.Crypto crypto => Addr crypto -> Gen Property
propDecompactErrors addr = do
  let (CA.UnsafeCompactAddr sbs) = CA.compactAddr addr
      hashLen = fromIntegral $ Hash.sizeHash (Proxy :: Proxy (CC.ADDRHASH crypto))
      bs = SBS.fromShort sbs
      flipHeaderBit b =
        case BS.uncons bs of
          Just (h, bsTail) -> BS.cons (complementBit h b) bsTail
          Nothing -> error "Impossible: CompactAddr can't be empty"
      mingleHeader = do
        b <- elements $ case addr of
          Addr {} -> [1, 2, 3, 7]
          AddrBootstrap {} -> [0 .. 7]
        pure ("Header", flipHeaderBit b)
      mingleAddLength = do
        NonEmpty xs <- arbitrary
        pure ("Add Length", bs <> BS.pack xs)
      mingleDropLength = do
        n <- chooseInt (1, BS.length bs)
        pure ("Drop Length", BS.take (BS.length bs - n) bs)
      mingleStaking = do
        let (prefix, suffix) = BS.splitAt (1 + hashLen) bs
            genBad32 =
              putVariableLengthWord64
                <$> chooseWord64 (fromIntegral (maxBound :: Word32) + 1, maxBound)
            genBad16 =
              putVariableLengthWord64
                <$> chooseWord64 (fromIntegral (maxBound :: Word16) + 1, maxBound)
            genGood32 =
              putVariableLengthWord64 . (fromIntegral :: Word32 -> Word64) <$> arbitrary
            genGood16 =
              putVariableLengthWord64 . (fromIntegral :: Word16 -> Word64) <$> arbitrary
            serializeSuffix xs = BSL.toStrict . B.runPut . mconcat <$> sequence xs
        case addr of
          Addr _ _ StakeRefPtr {} -> do
            newSuffix <-
              oneof
                [ serializeSuffix [genBad32, genGood16, genGood16],
                  serializeSuffix [genGood32, genBad16, genGood16],
                  serializeSuffix [genGood32, genGood16, genBad16],
                  serializeSuffix [genGood32, genGood16, genGood16, genGood16],
                  (\x -> BS.singleton (x .|. 0b10000000) <> suffix) <$> arbitrary
                ]
            pure ("Mingle Ptr", prefix <> newSuffix)
          Addr _ _ StakeRefNull {} -> do
            NonEmpty xs <- arbitrary
            pure ("Bogus Null Ptr", prefix <> BS.pack xs)
          Addr _ _ StakeRefBase {} -> do
            xs <- arbitrary
            let xs' = if length xs == hashLen then 0 : xs else xs
            pure ("Bogus Staking", prefix <> BS.pack xs')
          AddrBootstrap {} -> pure ("Bogus Bootstrap", BS.singleton 0b10000000 <> bs)
  (mingler, badAddr) <-
    oneof
      [ pure ("Empty", ""),
        mingleHeader,
        mingleAddLength,
        mingleDropLength,
        mingleStaking
      ]
  pure $
    counterexample
      ("Mingled address with " ++ mingler ++ "was parsed: " ++ show badAddr)
      $ isLeft $ CA.decodeAddrEither @crypto badAddr
