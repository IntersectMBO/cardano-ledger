{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.SafeHash (safeHashTest) where

import Cardano.Ledger.SafeHash
-- ByteString imports

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
-- Testing imports

import Data.Proxy
import Data.String (fromString)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes
import Test.Tasty
import Test.Tasty.HUnit

-- =========================
-- Some examples

data FooI -- HashAnnotated indexes which are analogs of our EraIndependentXXX

long :: ByteString
long = (fromString "abc")

short :: ShortByteString
short = toShort long

-- Any newtype over some type that is SafeToHash can easily
-- derive SafeToHash and also assign its HashAnnotated type, and
-- thus become a client of 'hashAnnotated'

newtype Foo c = Foo ShortByteString
  deriving (Show, SafeToHash)

instance HashAnnotated (Foo c) FooI c

foo :: Foo c
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

test1, test2, test3, test4 :: TestTree
test1 = testCase "short==long" (assertEqual "ShortByteString and ByteString don't hash the same" shorthash (castSafeHash longhash))
test2 = testCase "newtype==underlyingtype" (assertEqual "A newtype and its underlying type dont hash the same" shorthash (castSafeHash foohash))
test3 = testCase "short==long" (assertEqual "ShortByteString and ByteString don't hash the same" shorthashT (castSafeHash longhashT))
test4 = testCase "newtype==underlyingtype" (assertEqual "A newtype and its underlying type dont hash the same" shorthashT (castSafeHash foohashT))

safeHashTest :: TestTree
safeHashTest =
  testGroup
    "SafeHash"
    [ testGroup "StandardCrypto" [test1, test2],
      testGroup "TestCrypto" [test3, test4]
    ]
