{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Shelley.Spec.Ledger.SafeHash (safeHashTest) where

import Cardano.Ledger.SafeHash

-- Crypto imports
import Cardano.Crypto.DSIGN (Ed25519DSIGN, MockDSIGN)
import Cardano.Crypto.Hash (Blake2b_224, Blake2b_256, MD5Prefix)
import Cardano.Crypto.KES (MockKES, Sum6KES)
import Cardano.Crypto.VRF.Praos
import qualified Cardano.Ledger.Crypto as CryptoClass
import Shelley.Spec.Ledger.API (PraosCrypto)
import Test.Cardano.Crypto.VRF.Fake (FakeVRF)

-- ByteString imports
import Data.ByteString.Short (ShortByteString,toShort)
import Data.ByteString(ByteString)
import Data.String(fromString)

-- Testing imports
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy


-- =======================================================

data TestCrypto

instance CryptoClass.Crypto TestCrypto where
  type HASH TestCrypto = MD5Prefix 10
  type ADDRHASH TestCrypto = MD5Prefix 8
  type DSIGN TestCrypto = MockDSIGN
  type KES TestCrypto = MockKES 10
  type VRF TestCrypto = FakeVRF

instance PraosCrypto TestCrypto

data StandardCrypto

instance CryptoClass.Crypto StandardCrypto where
  type DSIGN StandardCrypto = Ed25519DSIGN
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF StandardCrypto = PraosVRF
  type HASH StandardCrypto = Blake2b_256
  type ADDRHASH StandardCrypto = Blake2b_224

instance PraosCrypto StandardCrypto

-- =========================
-- Some examples

data FooI  -- HashAnnotated indexes which are analogs of our EraIndependentXXX

long :: ByteString
long = (fromString "abc")

short :: ShortByteString
short = toShort long

-- Any newtype over some type that is SafeToHash can easily
-- derive SafeToHash and also assign its HashAnnotated type, and
-- thus become a client of 'hashAnnotated'

newtype Foo c = Foo ShortByteString
   deriving (Show,SafeToHash)
instance HashAnnotated (Foo c) FooI c

foo:: Foo c
foo = Foo short

-- ===================================
-- Lets run some examples, we'll need some concrete Crypto


foohash :: SafeHash StandardCrypto FooI
foohash = hashAnnotated foo

shorthash :: SafeHash StandardCrypto ShortByteString
shorthash = makeHashWithExplicitProxys (Proxy @StandardCrypto) (Proxy @ShortByteString) short

longhash :: SafeHash StandardCrypto ByteString
longhash = makeHashWithExplicitProxys (Proxy @StandardCrypto) (Proxy @ByteString) long

foohashT :: SafeHash TestCrypto FooI
foohashT = hashAnnotated foo

shorthashT :: SafeHash TestCrypto ShortByteString
shorthashT = makeHashWithExplicitProxys (Proxy @TestCrypto) (Proxy @ShortByteString) short

longhashT :: SafeHash TestCrypto ByteString
longhashT = makeHashWithExplicitProxys (Proxy @TestCrypto) (Proxy @ByteString) long

test1,test2,test3,test4 :: TestTree
test1 = testCase "short==long" (assertEqual "ShortByteString and ByteString don't hash the same" shorthash (castSafeHash longhash))
test2 = testCase "newtype==underlyingtype" (assertEqual "A newtype and its underlying type dont hash the same" shorthash (castSafeHash foohash))

test3 = testCase "short==long" (assertEqual "ShortByteString and ByteString don't hash the same" shorthashT (castSafeHash longhashT))
test4 = testCase "newtype==underlyingtype" (assertEqual "A newtype and its underlying type dont hash the same" shorthashT (castSafeHash foohashT))

safeHashTest :: TestTree
safeHashTest = testGroup "SafeHash"
                    [testGroup "StandardCrypto" [test1,test2]
                    ,testGroup "TestCrypto" [test3,test4]
                    ]
