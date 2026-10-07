{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Ledger.Api.Address (spec) where

import Cardano.Ledger.Api.Tx.Address
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Either (isLeft)
import Test.Cardano.Ledger.Common

spec :: Spec
spec = describe "Public address decoding" $ do
  let protectedBase = BS.cons 0x09 (BS.replicate 56 0)
      protectedPointer = BS.cons 0x49 (BS.replicate 31 0)
  it "accepts current protected bytes consistently across buffer and diagnostic APIs" $ do
    address <- either error pure (decodeAddrEither protectedBase)
    (decodeAddr protectedBase :: Maybe Addr) `shouldBe` Just address
    decodeAddrShortEither (SBS.toShort protectedBase) `shouldBe` Right address
    (decodeAddrShort (SBS.toShort protectedBase) :: Maybe Addr) `shouldBe` Just address
    (decodeAddrLenient protectedBase :: Maybe Addr) `shouldBe` Just address
    decodeAddrLenientEither protectedBase `shouldBe` Right (DecAddr address)
  it "rejects protected pointers through every public parser" $ do
    decodeAddrEither protectedPointer `shouldSatisfy` isLeft
    (decodeAddr protectedPointer :: Maybe Addr) `shouldBe` Nothing
    decodeAddrShortEither (SBS.toShort protectedPointer) `shouldSatisfy` isLeft
    (decodeAddrShort (SBS.toShort protectedPointer) :: Maybe Addr) `shouldBe` Nothing
    (decodeAddrLenient protectedPointer :: Maybe Addr) `shouldBe` Nothing
    decodeAddrLenientEither protectedPointer `shouldSatisfy` isLeft
  it "retains legacy diagnostic distinctions for malformed ordinary bytes" $ do
    let ordinaryBase = BS.cons 0x01 (BS.replicate 56 0)
        oversizedPointer = BS.cons 0x41 (BS.replicate 28 0) <> BS.pack [0x90, 0x80, 0x80, 0x80, 0, 0, 0]
        normalizedPointer = BS.cons 0x41 (BS.replicate 31 0)
    baseAddr <- either error pure (decodeAddrEither ordinaryBase)
    pointerAddr <- either error pure (decodeAddrEither normalizedPointer)
    decodeAddrLenientEither (ordinaryBase <> "junk")
      `shouldBe` Right (DecAddrUnconsumed baseAddr "junk")
    decodeAddrLenientEither oversizedPointer `shouldBe` Right (DecAddrBadPtr pointerAddr)
